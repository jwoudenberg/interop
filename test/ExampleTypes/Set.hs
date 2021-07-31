module ExampleTypes.Set (example, gen) where

import qualified Data.Set as Set
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Set.Set Int
example = Set.fromList [1, 3, 5]

gen :: Hedgehog.Gen (Set.Set Int)
gen =
  Set.fromList
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
