module ExampleTypes.Word64 (example, gen) where

import Data.Word (Word64)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Word64
example = 4

gen :: Hedgehog.Gen Word64
gen = Gen.word64 Range.exponentialBounded

-- JSON encoding of example value:
--
-- 4
