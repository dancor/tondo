module Main where

-- a merger for sorted key-value pairs

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Word
import FUtil
import System.Environment
import System.Exit
import qualified Control.LolLog as Log
import qualified Data.ByteString as B
import qualified Data.Map as M

type Content = B.ByteString
type KType = Content
type VType = Content

data ConflType a =
  AddAdd a a |
  AddChange a |
  ChangeAdd a |
  ChangeChange a a
  deriving (Show)

data MergeRes a =
  MergeOk a |
  MergeConfl (ConflType a)
  deriving (Show)

data DiffType a = DiffAdd a | DiffDel a | DiffChange a a
  deriving (Show)

byteNl, byteTab, byteBs :: Word8
byteNl = fromIntegral $ ord '\n'
byteTab = fromIntegral $ ord '\t'
byteBs = fromIntegral $ ord '\\'

-- clowntown?
esc :: Content -> Content
esc = B.concatMap $ \ x -> if x == byteBs
  then B.replicate 2 byteBs
  else
    if x == byteTab
      then B.singleton byteBs `B.append` B.singleton byteTab
      else
        if x == byteNl
          then B.singleton byteBs `B.append` B.singleton byteNl
          else B.singleton x

unEsc :: Content -> Content
unEsc = snd . B.foldl f (False, B.empty) where
  f (True, pre) c = (False, pre `B.append` B.singleton c)
  f (False, pre) c =
    if c == byteBs then (True, pre) else (False, pre `B.append` B.singleton c)

parsePairs :: Content -> M.Map KType VType
parsePairs = M.fromList .
  map (bothond unEsc . second B.tail . B.breakByte byteTab) .
  init . B.split byteNl

unparsePairs :: [(KType, VType)] -> Content
unparsePairs = B.concat . map (\ (k, v) -> esc k `B.append`
  B.singleton byteTab `B.append` esc v `B.append` B.singleton byteNl)

diffMap :: M.Map KType VType -> M.Map KType VType ->
  M.Map KType (DiffType VType)
diffMap a b = M.mapMaybe g $ M.unionWith f (M.map DiffDel a) (M.map DiffAdd b)
  where
  f (DiffDel x) (DiffAdd y) = DiffChange x y
  g a@(DiffChange x y) = if x == y then Nothing else Just a

mergeDiffMaps :: M.Map KType VType -> M.Map KType (DiffType VType) ->
  M.Map KType (DiffType VType) -> M.Map KType (MergeRes VType)
mergeDiffMaps orig a b = M.unionWith const
    (M.mapMaybe g $ M.unionWith f (M.map Right a) (M.map Right b))
    (M.map MergeOk orig)
  where
  f a@(Right (DiffAdd x)) (Right (DiffAdd y)) =
    if x == y then a else Left (AddAdd x y)
  f a@(Right (DiffAdd x)) (Right (DiffDel _)) = a
  f a@(Right (DiffAdd x)) (Right (DiffChange _ y)) =
    if x == y then a else Left (AddChange y)
  f (Right (DiffDel _)) b@(Right (DiffAdd _)) = b
  f (Right (DiffDel _)) b@(Right (DiffDel _)) = b
  f (Right (DiffDel _)) b@(Right (DiffChange _ _)) = b
  f a@(Right (DiffChange _ x)) (Right (DiffAdd y)) =
    if x == y then a else Left (ChangeAdd x)
  f a@(Right (DiffChange _ x)) (Right (DiffChange _ y)) =
    if x == y then a else Left (ChangeChange x y)
  f a@(Right (DiffChange _ _)) (Right (DiffDel _)) = a
  g (Right (DiffDel _)) = Nothing
  g (Right (DiffAdd x)) = Just $ MergeOk x
  g (Right (DiffChange _ x)) = Just $ MergeOk x
  g (Left c) = Just $ MergeConfl c

showMerge (MergeOk a) = [a]
showMerge (MergeConfl (AddAdd a b)) = [a, b]
showMerge (MergeConfl (AddChange a)) = [B.empty, a]
showMerge (MergeConfl (ChangeAdd a)) = [a, B.empty]
showMerge (MergeConfl (ChangeChange a b)) = [a, b]

isConflict (MergeOk _) = False
isConflict (MergeConfl _) = True

mapMergeDriver :: Content -> Content -> Content -> (Content, Bool)
mapMergeDriver parent current other = (merge, gotConflicts) where
  pM = parsePairs parent
  cM = parsePairs current
  oM = parsePairs other
  pToCM = diffMap pM cM
  pToOM = diffMap pM oM
  mM = mergeDiffMaps pM pToCM pToOM
  gotConflicts = any (isConflict . snd) $ M.toList mM
  merge = unparsePairs . concatMap f . M.toList $ M.map showMerge mM
  f (a, strs) = map ((,) a) strs

on3 :: (a -> a -> a -> b) -> (c -> a) -> c -> c -> c -> b
on3 f g x y z = f (g x) (g y) (g z)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [parentFN, currentFN, otherFN] -> do
      (merge, gotConflicts) <- on3 (liftM3 mapMergeDriver) B.readFile
        parentFN currentFN otherFN
      B.writeFile currentFN merge
      if gotConflicts then exitFailure else exitSuccess
    _ -> error "usage"

