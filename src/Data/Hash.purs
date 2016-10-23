-- | `Data.Hash` provides non-cryptographic hash functions for various data
-- | types.
module Data.Hash
( class Hash
, hash
, hashFoldable
, combine
, gHash

, WithHashCache
, withHashCache
, withoutHashCache
) where

import Data.Array (sortBy)
import Data.Char as Char
import Data.Foldable (class Foldable, foldl)
import Data.Generic (class Generic, GenericSpine(..), toSpine)
import Data.Lazy (defer, Lazy, force)
import Data.String as String
import Prelude

-- | Instances must satisfy the following laws:
-- |
-- | - Equality: `not (x == y) || hash x == hash y`
class Hash a where
  hash :: a -> Int

instance hashGenericSpine :: Hash GenericSpine where
  hash (SProd name fields) =
    combine (hash name) (hashFoldable $ fields # map (_ $ unit))
  hash (SRecord fields) = combine (hashFoldable keys) (hashFoldable vals)
    where sortedFields = fields # sortBy (\a b -> a.recLabel `compare` b.recLabel)
          keys = sortedFields # map _.recLabel
          vals = sortedFields # map (_.recValue >>> (_ $ unit))
  hash (SNumber v)  = hash $ show v
  hash (SBoolean v) = if v then 1 else 1073741823
  hash (SInt v)     = v
  hash (SString s)  = hashFoldable $ String.toCharArray s
  hash (SChar c)    = Char.toCharCode c
  hash (SArray xs)  = hashFoldable $ xs # map (_ $ unit)
  hash SUnit        = 23121994

instance hashUnit    :: Hash Unit  where hash = gHash
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

-- | Pair a value with a cache of its hash.
data WithHashCache a = WithHashCache (Lazy Int) a

-- | Pair a value with a cache of its hash. The hash is computed
-- | lazily.
withHashCache :: forall a. (Hash a) => a -> WithHashCache a
withHashCache x = WithHashCache (defer \_ -> hash x) x

-- | Extract the value from a value paired with a cache of its
-- | hash.
withoutHashCache :: forall a. WithHashCache a -> a
withoutHashCache (WithHashCache _ x) = x

instance eqWithHashCache :: (Eq a) => Eq (WithHashCache a) where
  eq a b = withoutHashCache a == withoutHashCache b

instance hashWithHashCache :: Hash (WithHashCache a) where
  hash (WithHashCache h _) = force h
