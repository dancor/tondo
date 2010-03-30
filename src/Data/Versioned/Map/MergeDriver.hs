#!/usr/bin/env runhaskell

module Main where

-- a merger for sorted key-value pairs

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Prelude hiding (log)
import System.Environment
import System.Exit
import qualified Control.LolLog as Log
import qualified Data.ByteString as B
import qualified Data.Map as Map

log name x = Log.log "map-merge-driver" "debug" $ name ++ ": " ++ show x

-- take out of IO after done with logging
mapMergeDriver :: B.ByteString -> B.ByteString -> B.ByteString ->
  IO (B.ByteString, Bool)
mapMergeDriver parent current other = do
  let
    readMap = toMap . init . B.split nl
    toMap = Map.fromList . map (second B.tail . B.span (/= tb))
    nl = fromIntegral $ ord '\n'
    tb = fromIntegral $ ord '\t'
    p = readMap parent
    c = readMap current
    o = readMap other
  log "parent" $ show p
  log "current" $ show c
  log "other" $ show o
  --return (parent, True)
  return (current, True)

on3 f g a b c = f (g a) (g b) (g c)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [parentFN, currentFN, otherFN] -> do
      (merge, gotConflicts) <- on3 (liftM3 mapMergeDriver) B.readFile
        parentFN currentFN otherFN
      if gotConflicts then exitFailure else exitSuccess
    _ -> error "usage"
