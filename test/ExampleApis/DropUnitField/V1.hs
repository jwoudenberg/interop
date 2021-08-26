{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.DropUnitField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop
import qualified Interop.Wire as Wire

data DropUnitFieldType = DropUnitFieldType
  { field :: Int,
    unitField :: ()
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire DropUnitFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint
      "DropUnitField"
      (\(req :: DropUnitFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen DropUnitFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure $ DropUnitFieldType int ()
