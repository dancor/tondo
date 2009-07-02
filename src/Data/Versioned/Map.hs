{-# LANGUAGE TemplateHaskell #-}

module Data.Versioned.Map where

import Control.Monad
import Data.Binary
import Data.DeriveTH
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Internal as BSI

data Action k v = AddKey k v | DelKey k
$(derive makeBinary ''Action)

-- Maybe later we will actually hash, but the atomic commits are likely to be
-- small enough that it's not worth it.
commitHash :: (Binary k, Binary v) => Action k v -> BSI.ByteString
commitHash = encode

data Commit = Commit

{-
replay :: [AtomicCommit k v] -> ([String], Map.Map k v)
replay
-}
