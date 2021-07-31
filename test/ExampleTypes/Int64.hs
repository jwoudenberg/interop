module ExampleTypes.Int64 (example, gen) where

import Data.Int (Int64)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Int64
example = 4

gen :: Hedgehog.Gen Int64
gen = Gen.int64 Range.exponentialBounded

-- JSON encoding of example value:
--
-- 4
