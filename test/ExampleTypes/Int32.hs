module ExampleTypes.Int32 (example, gen) where

import Data.Int (Int32)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Int32
example = 4

gen :: Hedgehog.Gen Int32
gen = Gen.int32 Range.exponentialBounded

-- JSON encoding of example value:
--
-- 4
