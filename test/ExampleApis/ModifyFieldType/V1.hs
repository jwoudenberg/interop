{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.ModifyFieldType.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop

data ModifyFieldTypeType = ModifyFieldTypeType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire ModifyFieldTypeType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "ModifyFieldType" (\(req :: ModifyFieldTypeType) -> pure req)
  ]

gen :: Hedgehog.Gen ModifyFieldTypeType
gen = do
  int <- Gen.int Range.exponentialBounded
  pure $ ModifyFieldTypeType int
