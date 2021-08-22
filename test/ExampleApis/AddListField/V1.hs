{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddListField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop
import qualified Interop.Wire as Wire

data AddListFieldType = AddListFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddListFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddListField" (\(req :: AddListFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddListFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure $ AddListFieldType int
