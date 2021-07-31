module ExampleTypes.Int16 (example, gen) where

import Data.Int (Int16)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Int16
example = 4

gen :: Hedgehog.Gen Int16
gen = Gen.int16 Range.exponentialBounded

-- JSON encoding of example value:
--
-- 4
