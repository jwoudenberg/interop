{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleTypes.NestedRecord (example, gen) where

import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop

data OuterRecord = OuterRecord {inner :: InnerRecord}
  deriving (Eq, Generic, Show)

instance Interop.Wire OuterRecord

data InnerRecord = InnerRecord
  { oneField :: Int,
    otherField :: Text
  }
  deriving (Eq, Generic, Show)

instance Interop.Wire InnerRecord

example :: OuterRecord
example = OuterRecord (InnerRecord 2 "Hi!")

gen :: Hedgehog.Gen OuterRecord
gen = do
  int <- Gen.int Range.exponentialBounded
  text <- Gen.text (Range.linear 0 100) Gen.unicode
  pure $ OuterRecord (InnerRecord int text)

-- JSON encoding of example value:
--
-- {
--     "inner": {
--         "oneField": 2,
--         "otherField": "Hi!"
--     }
-- }
