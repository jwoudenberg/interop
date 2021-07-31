module ExampleTypes.Word8 (example, gen) where

import Data.Word (Word8)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Word8
example = 4

gen :: Hedgehog.Gen Word8
gen = Gen.word8 Range.exponentialBounded

-- JSON encoding of example value:
--
-- 4
