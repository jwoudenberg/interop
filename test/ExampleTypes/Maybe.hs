module ExampleTypes.Maybe (example, gen) where

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Maybe Int
example = Nothing

gen :: Hedgehog.Gen (Maybe Int)
gen = Gen.maybe (Gen.int Range.exponentialBounded)

-- JSON encoding of example value:
--
-- null
