module ExampleTypes.Bool (example, gen) where

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen

example :: Bool
example = True

gen :: Hedgehog.Gen Bool
gen = Gen.bool

-- JSON encoding of example value:
--
-- true
