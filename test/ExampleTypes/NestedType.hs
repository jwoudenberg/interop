{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleTypes.NestedType (example, gen) where

import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop.Wire as Wire

data NestedType = NestedType Record
  deriving (Eq, Generic, Show)

instance Wire.Wire NestedType

data Record = Record
  { oneField :: Int,
    otherField :: Text
  }
  deriving (Eq, Generic, Show)

instance Wire.Wire Record

example :: NestedType
example = NestedType (Record 2 "Hi!")

gen :: Hedgehog.Gen NestedType
gen = do
  int <- Gen.int Range.exponentialBounded
  text <- Gen.text (Range.linear 0 100) Gen.unicode
  pure $ NestedType (Record int text)

-- JSON encoding of example value:
--
-- {
--     "NestedType": {
--         "oneField": 2,
--         "otherField": "Hi!"
--     }
-- }
