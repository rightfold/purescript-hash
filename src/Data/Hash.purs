module Data.Hash
( class Hash
, hash

, combine
) where

import Data.Char as Char
import Data.Foldable (foldl)
import Data.String as String
import Prelude

class Hash a where
  hash :: a -> Int

instance hashInt :: Hash Int where
  hash = id

instance hashString :: Hash String where
  hash = String.toCharArray >>> map Char.toCharCode >>> foldl combine 0

combine :: Int -> Int -> Int
combine x y = x + 31 * x + y
