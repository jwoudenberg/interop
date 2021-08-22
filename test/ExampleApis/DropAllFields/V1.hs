{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.DropAllFields.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop
import qualified Interop.Wire as Wire

data DropAllFieldsType
  = DropAllFieldsFirstConstructor Record
  | DropAllFieldsSecondConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire DropAllFieldsType

data Record = Record
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire Record

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "DropAllFields" (\(req :: DropAllFieldsType) -> pure req)
  ]

gen :: Hedgehog.Gen DropAllFieldsType
gen =
  Gen.choice
    [ do
        int <- Gen.int Range.exponentialBounded
        pure $ DropAllFieldsFirstConstructor (Record int),
      pure DropAllFieldsSecondConstructor
    ]
