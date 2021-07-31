module ExampleTypes.Word (example, gen) where

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Word
example = 4

gen :: Hedgehog.Gen Word
gen = Gen.word Range.exponentialBounded

-- JSON encoding of example value:
--
-- 4
