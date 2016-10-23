module Test.Main
( main
) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic (class Generic)
import Data.Hash
import Prelude
import Test.Assert (ASSERT, assert)

data Structure
  = One
  | Two Boolean
  | Four1 Boolean Boolean
  | Four2 Boolean Boolean
  | Record {a :: Int, b :: Int}

derive instance genericStructure :: Generic Structure

instance hashStructure :: Hash Structure where hash = gHash

main :: forall eff. Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
main = do
  pure $ hash unit
  pure $ hash 0.0
  pure $ hash true
  pure $ hash 0
  pure $ hash ""
  pure $ hash 'c'
  pure $ hash [0]
  pure $ hashFoldable [0]

  test $ hash 1.0 /= hash 1.1
  test $ hash nan == hash nan

  test $ hash true /= hash false

  test $ hash 0 == 0
  test $ hash 1 == 1
  test $ hash (top :: Int) == top
  test $ hash (bottom :: Int) == bottom

  test $ hash "" /= hash "a"
  test $ hash "a" /= hash "b"
  test $ hash "a" /= hash "aa"
  test $ hash "foobar" /= hash "bazqux"

  test $ hash 'a' /= hash 'b'

  test $ hash ([] :: Array Int) /= hash [0]
  test $ hash [1] /= hash [1, 2, 3]

  test $ hash One /= hash (Two true)
  test $ hash (Two true) /= hash (Two false)
  test $ hash (Four1 false true) /= hash (Four1 true false)
  test $ hash (Four1 false true) /= hash (Four2 false true)
  test $ hash (Record {a: 1, b: 2}) /= hash (Record {a: 2, b: 1})

  test $ isGoodInt (combine top top)
  test $ isGoodInt (combine bottom bottom)
  test $ isGoodInt (combine top bottom)
  test $ isGoodInt (combine bottom top)

  test $ withoutHashCache (withHashCache 42) == 42
  test $ hash (withHashCache 42) == hash 42

test :: forall eff. Boolean -> Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit
test c = do
  assert c
  log "ok"

isGoodInt :: Int -> Boolean
isGoodInt n = isGoodIntImpl bottom top n

foreign import isGoodIntImpl :: Int -> Int -> Int -> Boolean

foreign import nan :: Number
