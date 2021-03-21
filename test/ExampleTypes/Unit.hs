module ExampleTypes.Unit (example, gen) where

import qualified Hedgehog

example :: ()
example = ()

gen :: Hedgehog.Gen ()
gen = pure ()

-- JSON encoding of example value:
--
-- []
