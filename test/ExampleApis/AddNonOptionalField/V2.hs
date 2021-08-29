{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddNonOptionalField.V2 where

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
  [ Interop.endpoint "AddNonOptionalField" (\(req :: AddNonOptionalFieldType) -> pure req)
  ]

data AddNonOptionalFieldType = AddNonOptionalFieldType
  { field :: Int,
    newField :: Int
  }
  deriving (Generic)

instance Wire.Wire AddNonOptionalFieldType

-- Warnings when V1 is used by a server and V2 by a client:
--
-- A type used in requests has a mandatory field.
-- data AddNonOptionalFieldType { newField }
--
-- This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First add an optional field. Then update clients to always set the optional field. Finally make the new field non-optional.
