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
-- The generated client code expects record 'AddNonOptionalFieldType' to have a field 'newField', but server responses don't include such a field.
-- data AddNonOptionalFieldType { newField }
--
-- Maybe you're trying to remove a field from a record? If so, make sure to follow these steps:
--
-- 1. Make the field optional by wrapping it in a 'Maybe', but don't return 'Nothing' values from the server yet.
-- 2. Change the client to make it support responses that omit the field.
-- 3. Make sure the changes from step 1 and 2 are deployed.
-- 4. Remove the field from the server code.
--
-- It looks like you missed step 1, because 'newField' isn't an optional field.
