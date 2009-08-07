#!/usr/bin/env runhaskell

module Main where

-- a merger for sorted key-value pairs

import Control.Applicative
import Data.Char
import Prelude hiding (log)
import System.Environment
import System.Exit
import qualified Control.LolLog as Log
import qualified Data.ByteString as B

log name x = Log.log "map-merge-driver" "debug" $ name ++ ": " ++ show x

mapMergeDriver :: [B.ByteString] -> [B.ByteString] -> [B.ByteString] ->
  IO ([B.ByteString], Bool)
mapMergeDriver parent current other = do
  log "parent" parent
  log "current" current
  log "other" other
  --return (parent, True)
  return (current, True)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [parentFN, currentFN, otherFN] -> do
      let
        readLines = fmap (B.split nl) . B.readFile
        nl = fromIntegral $ ord '\n'
      parent  <- readLines parentFN
      current <- readLines currentFN
      other   <- readLines otherFN
      (merge, gotConflicts) <- mapMergeDriver parent current other
      B.writeFile currentFN $ B.intercalate (B.pack [nl]) merge
      if gotConflicts then exitFailure else exitSuccess
    _ -> error "usage"
