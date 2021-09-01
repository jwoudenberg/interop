{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.DropNonOptionalField.V2 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "DropNonOptionalField" (\(req :: DropNonOptionalFieldType) -> pure req)
  ]

data DropNonOptionalFieldType = DropNonOptionalFieldType
  { field1 :: Int
  }
  deriving (Generic)

instance Interop.Wire DropNonOptionalFieldType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- The generated client code expects record 'DropNonOptionalFieldType' to have a field 'field2', but server responses don't include such a field.
-- data DropNonOptionalFieldType { field2 }
--
-- Maybe you're trying to remove a field from a record? If so, make sure to follow these steps:
--
-- 1. Make the field optional by wrapping it in a 'Maybe', but don't return 'Nothing' values from the server yet.
-- 2. Change the client to make it support responses that omit the field.
-- 3. Make sure the changes from step 1 and 2 are deployed.
-- 4. Remove the field from the server code.
--
-- It looks like you missed step 1, because 'field2' isn't an optional field.
