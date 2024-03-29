{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.DropNonOptionalField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop

data DropNonOptionalFieldType = DropNonOptionalFieldType
  { field1 :: Int,
    field2 :: Int
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire DropNonOptionalFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "DropNonOptionalField" (\(req :: DropNonOptionalFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen DropNonOptionalFieldType
gen = do
  int1 <- Gen.int Range.exponentialBounded
  int2 <- Gen.int Range.exponentialBounded
  pure $ DropNonOptionalFieldType int1 int2
