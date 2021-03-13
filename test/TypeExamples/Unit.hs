module TypeExamples.Unit (example, gen) where

import qualified Hedgehog

example :: ()
example = ()

gen :: Hedgehog.Gen ()
gen = pure ()

-- []
