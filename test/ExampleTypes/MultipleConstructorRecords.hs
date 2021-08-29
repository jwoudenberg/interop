{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module ExampleTypes.MultipleConstructorRecords (example, gen) where

import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop.Wire as Wire

data Footwear
  = Shoe
      { material :: Text,
        laces :: Int
      }
  | Sock
      { patterned :: Bool,
        holes :: Int
      }
  deriving (Eq, Generic, Show)

instance Wire.Wire Footwear

example :: Footwear
example = Sock True 1

gen :: Hedgehog.Gen Footwear
gen =
  Gen.choice
    [ Shoe
        <$> Gen.text (Range.linear 0 100) Gen.unicode
        <*> Gen.int Range.exponentialBounded,
      Sock
        <$> Gen.bool
        <*> Gen.int Range.exponentialBounded
    ]

-- JSON encoding of example value:
--
-- {
--     "Sock": {
--         "holes": 1,
--         "patterned": true
--     }
-- }
