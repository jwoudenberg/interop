{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddHashMapField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop

data AddHashMapFieldType = AddHashMapFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire AddHashMapFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddHashMapField" (\(req :: AddHashMapFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddHashMapFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure $ AddHashMapFieldType int
