{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleTypes.Record (example, gen) where

import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop

data Record = Record
  { oneField :: Int,
    otherField :: Text
  }
  deriving (Eq, Generic, Show)

instance Interop.Wire Record

example :: Record
example = Record 2 "Hi!"

gen :: Hedgehog.Gen Record
gen =
  Record
    <$> Gen.int Range.exponentialBounded
    <*> Gen.text (Range.linear 0 100) Gen.unicode

-- JSON encoding of example value:
--
-- {
--     "oneField": 2,
--     "otherField": "Hi!"
-- }
