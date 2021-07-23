module ExampleTypes.Dict (example, gen) where

import qualified Data.Map.Strict as Map
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Map.Map Int Bool
example = Map.fromList [(1, True), (5, False)]

gen :: Hedgehog.Gen (Map.Map Int Bool)
gen =
  Map.fromList
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
