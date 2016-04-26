module Test.Main
( main
) where

import Control.Monad.Eff (Eff)
import Data.Hash
import Prelude
import Test.Assert (ASSERT, assert)

main :: forall eff. Eff (assert :: ASSERT | eff) Unit
main = do
  pure $ hash unit
  pure $ hash 0.0
  pure $ hash true
  pure $ hash 0
  pure $ hash ""
  pure $ hash 'c'
  pure $ hash [0]

  assert $ isGoodInt (combine top top)
  assert $ isGoodInt (combine bottom bottom)
  assert $ isGoodInt (combine top bottom)
  assert $ isGoodInt (combine bottom top)

isGoodInt :: Int -> Boolean
isGoodInt n = isGoodIntImpl bottom top n

foreign import isGoodIntImpl :: Int -> Int -> Int -> Boolean
