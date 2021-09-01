{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.DropOptionalField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop

data DropOptionalFieldType = DropOptionalFieldType
  { field :: Int,
    optionalField :: Maybe Int
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire DropOptionalFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint
      "DropOptionalField"
      (\(req :: DropOptionalFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen DropOptionalFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  maybeInt <- Gen.maybe $ Gen.int Range.exponentialBounded
  pure $ DropOptionalFieldType int maybeInt
