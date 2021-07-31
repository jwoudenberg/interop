module ExampleTypes.Seq (example, gen) where

import qualified Data.Sequence as Seq
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Seq.Seq Int
example = Seq.fromList [1, 3, 5]

gen :: Hedgehog.Gen (Seq.Seq Int)
gen =
  Seq.fromList
    <$> Gen.list
      (Range.linear 0 100)
      (Gen.int Range.exponentialBounded)

-- JSON encoding of example value:
--
-- [
--     1,
--     3,
--     5
-- ]
