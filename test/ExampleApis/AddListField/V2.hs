{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddListField.V2 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddListField" (\(req :: AddListFieldType) -> pure req)
  ]

data AddListFieldType = AddListFieldType
  { field :: Int,
    otherListField :: [Int]
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddListFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
