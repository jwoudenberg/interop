module ExampleTypes.Float (example, gen) where

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Float
example = 4.1

gen :: Hedgehog.Gen Float
gen = Gen.float (Range.exponentialFloat 0 1000)

-- JSON encoding of example value:
--
-- 4.1
