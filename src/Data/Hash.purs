module Data.Hash
( class Hash
, hash

, combine
) where

import Data.Char as Char
import Data.Foldable (class Foldable, foldl)
import Data.String as String
import Prelude

class Hash a where
  hash :: a -> Int

instance hashUnit :: Hash Unit where
  hash _ = 1

instance hashBoolean :: Hash Boolean where
  hash true = 1
  hash false = 1073741823

instance hashInt :: Hash Int where
  hash = id

instance hashNumber :: Hash Number where
  hash = numberToString >>> hash
foreign import numberToString :: Number -> String

instance hashFoldable :: (Foldable f, Hash a) => Hash (f a) where
  hash = foldl (\h x -> combine h (hash x)) 1

instance hashChar :: Hash Char where
  hash = Char.toCharCode

instance hashString :: Hash String where
  hash = String.toCharArray >>> hash

combine :: Int -> Int -> Int
combine x y = x + 31 * x + y
