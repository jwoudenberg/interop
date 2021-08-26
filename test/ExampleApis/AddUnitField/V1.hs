{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddUnitField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop
import qualified Interop.Wire as Wire

data AddUnitFieldType = AddUnitFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddUnitFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddUnitField" (\(req :: AddUnitFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddUnitFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure $ AddUnitFieldType int
