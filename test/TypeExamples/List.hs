module TypeExamples.List (example, gen) where

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: [Int]
example = [1, 2, 3]

gen :: Hedgehog.Gen [Int]
gen = Gen.list (Range.linear 0 100) (Gen.int Range.exponentialBounded)

-- JSON encoding of example value:
--
-- [
--     1,
--     2,
--     3
-- ]
