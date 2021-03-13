{-# LANGUAGE OverloadedStrings #-}

module TypeExamples.Text (example, gen) where

import Data.Text (Text)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

example :: Text
example = "Hi!"

gen :: Hedgehog.Gen Text
gen = Gen.text (Range.linear 0 100) Gen.unicode

-- JSON encoding of example value:
--
-- "Hi!"
