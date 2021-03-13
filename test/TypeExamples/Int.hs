module TypeExamples.Int (example, gen) where

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Int
example = 4

gen :: Hedgehog.Gen Int
gen = Gen.int Range.exponentialBounded

-- 4
