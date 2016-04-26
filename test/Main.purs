module Test.Main
( main
) where

import Control.Monad.Eff (Eff)
import Data.Hash
import Prelude
import Test.Assert (ASSERT, assert)

main :: forall eff. Eff (assert :: ASSERT | eff) Unit
main = do
  assert $ isGoodInt (combine top top)
  assert $ isGoodInt (combine bottom bottom)
  assert $ isGoodInt (combine top bottom)
  assert $ isGoodInt (combine bottom top)

isGoodInt :: Int -> Boolean
isGoodInt n = n >= bottom && n <= top
