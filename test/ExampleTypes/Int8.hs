module ExampleTypes.Int8 (example, gen) where

import Data.Int (Int8)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Int8
example = 4

gen :: Hedgehog.Gen Int8
gen = Gen.int8 Range.exponentialBounded

-- JSON encoding of example value:
--
-- 4
