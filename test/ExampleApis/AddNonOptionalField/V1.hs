{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddNonOptionalField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop
import qualified Interop.Wire as Wire

data AddNonOptionalFieldType = AddNonOptionalFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddNonOptionalFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddNonOptionalField" (\(req :: AddNonOptionalFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddNonOptionalFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure (AddNonOptionalFieldType int)
