{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddMapField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop

data AddMapFieldType = AddMapFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire AddMapFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddMapField" (\(req :: AddMapFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddMapFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure $ AddMapFieldType int
