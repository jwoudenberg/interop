module ExampleTypes.Word16 (example, gen) where

import Data.Word (Word16)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Word16
example = 4

gen :: Hedgehog.Gen Word16
gen = Gen.word16 Range.exponentialBounded

-- JSON encoding of example value:
--
-- 4
