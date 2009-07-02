module Data.Guid where

import Control.Applicative
import Control.Arrow
import Control.Monad.Random
import Data.Char
import Numeric

data Guid = Guid Integer
  deriving (Eq, Ord)

instance Show Guid where
  show (Guid i) = (\ s -> replicate (32 - length s) '0' ++ s) $ showHex i ""

instance Read Guid where
  readsPrec _ = map (first Guid) . readHex . dropWhile isSpace

newGuid :: (RandomGen g) => Rand g Guid
newGuid = Guid <$> getRandomR (0, 2 ^ 128 - 1)
