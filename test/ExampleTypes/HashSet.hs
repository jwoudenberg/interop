module ExampleTypes.HashSet (example, gen) where

import qualified Data.HashSet as HashSet
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: HashSet.HashSet Int
example = HashSet.fromList [1, 3, 5]

gen :: Hedgehog.Gen (HashSet.HashSet Int)
gen =
  HashSet.fromList
    <$> Gen.list
      (Range.linear 0 100)
      (Gen.int Range.exponentialBounded)

-- JSON encoding of example value:
--
-- [
--     1,
--     3,
--     5
-- ]
