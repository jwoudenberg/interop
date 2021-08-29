{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.DropAllFields.V2 where

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
  [ Interop.endpoint "DropAllFields" (\(req :: DropAllFieldsType) -> pure req)
  ]

data DropAllFieldsType
  = DropAllFieldsFirstConstructor
  | DropAllFieldsSecondConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire DropAllFieldsType

-- Warnings when V1 is used by a server and V2 by a client:
--
-- The server expects record 'DropAllFieldsType' to have a field 'field', but client requests don't include such a field.
-- data DropAllFieldsType = DropAllFieldsFirstConstructor { field }
--
-- Maybe you're trying to add a new field to a record? If so, make sure to follow these steps:
--
-- 1. Add the field to the server, but wrap it in a 'Maybe' to keep it optional for now.
-- 2. Change the client to always send the new field.
-- 3. Make sure the changes from step 1 and 2 are deployed.
-- 4. Make the field mandatory in the server by removing the 'Maybe'.
--
-- It looks like you missed step 1, because 'field' isn't an optional field.
