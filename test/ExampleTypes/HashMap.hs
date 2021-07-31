module ExampleTypes.HashMap (example, gen) where

import qualified Data.HashMap.Strict as HashMap
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: HashMap.HashMap Int Bool
example = HashMap.fromList [(1, True), (5, False)]

gen :: Hedgehog.Gen (HashMap.HashMap Int Bool)
gen =
  HashMap.fromList
    <$> Gen.list
      (Range.linear 0 100)
      ((,) <$> Gen.int Range.exponentialBounded <*> Gen.bool)

-- JSON encoding of example value:
--
-- [
--     [
--         1,
--         true
--     ],
--     [
--         5,
--         false
--     ]
-- ]
