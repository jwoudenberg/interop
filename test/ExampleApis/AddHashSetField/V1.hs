{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddHashSetField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop

data AddHashSetFieldType = AddHashSetFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire AddHashSetFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddHashSetField" (\(req :: AddHashSetFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddHashSetFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure $ AddHashSetFieldType int
