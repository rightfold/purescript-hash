-- | `Data.Hash` provides non-cryptographic hash functions for various data
-- | types.
module Data.Hash
( class Hash
, hash
, hashFoldable
, combine
, gHash
) where

import Data.Array (sortBy)
import Data.Char as Char
import Data.Foldable (class Foldable, foldl)
import Data.Generic (class Generic, GenericSpine(..), toSpine)
import Data.String as String
import Prelude

class Hash a where
  hash :: a -> Int

instance hashGenericSpine :: Hash GenericSpine where
  hash (SProd name fields) =
    combine (hash name) (hashFoldable $ fields # map (_ $ unit))
  hash (SRecord fields) = combine (hashFoldable keys) (hashFoldable vals)
    where sortedFields = fields # sortBy (\a b -> a.recLabel `compare` b.recLabel)
          keys = sortedFields # map _.recLabel
          vals = sortedFields # map (_.recValue >>> (_ $ unit))
  hash (SNumber v)  = hash $ numberToString v
  hash (SBoolean v) = if v then 1 else 1073741823
  hash (SInt v)     = v
  hash (SString s)  = hashFoldable $ String.toCharArray s
  hash (SChar c)    = Char.toCharCode c
  hash (SArray xs)  = hashFoldable $ xs # map (_ $ unit)

instance hashUnit :: Hash Unit where
  hash _ = 1

instance hashNumber  :: Hash Number  where hash = gHash
instance hashBoolean :: Hash Boolean where hash = gHash
instance hashInt     :: Hash Int     where hash = gHash
instance hashString  :: Hash String  where hash = gHash
instance hashChar    :: Hash Char    where hash = gHash

instance hashArray :: (Hash a) => Hash (Array a) where hash = hashFoldable

-- | Hash any foldable of hashable values.
hashFoldable :: forall f a. (Foldable f, Hash a) => f a -> Int
hashFoldable = foldl (\h x -> combine h (hash x)) 1

-- | Combine two hash values into a single hash value.
combine :: Int -> Int -> Int
combine x y = x + 31 * x + y

-- | Generic hash function.
gHash :: forall a. (Generic a) => a -> Int
gHash = toSpine >>> hash

foreign import numberToString :: Number -> String
