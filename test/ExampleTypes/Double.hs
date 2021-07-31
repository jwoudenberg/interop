module ExampleTypes.Double (example, gen) where

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Double
example = 4.1

gen :: Hedgehog.Gen Double
gen = Gen.double (Range.exponentialFloat 0 1000)

-- JSON encoding of example value:
--
-- 4.1
