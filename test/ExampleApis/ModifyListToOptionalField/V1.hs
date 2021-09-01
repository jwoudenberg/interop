{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.ModifyListToOptionalField.V1 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Interop

data ModifyListToOptionalFieldType = ModifyListToOptionalFieldType
  { listField :: [Int]
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire ModifyListToOptionalFieldType

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "ModifyListToOptionalField" (\(req :: ModifyListToOptionalFieldType) -> pure req)
  ]

gen :: Hedgehog.Gen ModifyListToOptionalFieldType
gen = do
  listInt <- Gen.list (Range.linear 0 100) (Gen.int Range.exponentialBounded)
  pure $ ModifyListToOptionalFieldType listInt
