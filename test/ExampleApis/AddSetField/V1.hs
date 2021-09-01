{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddSetField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop

data AddSetFieldType = AddSetFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire AddSetFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddSetField" (\(req :: AddSetFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen AddSetFieldType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure $ AddSetFieldType int
