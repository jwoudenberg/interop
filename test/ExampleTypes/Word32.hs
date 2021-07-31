module ExampleTypes.Word32 (example, gen) where

import Data.Word (Word32)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Word32
example = 4

gen :: Hedgehog.Gen Word32
gen = Gen.word32 Range.exponentialBounded

-- JSON encoding of example value:
--
-- 4
