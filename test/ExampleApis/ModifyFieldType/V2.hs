{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.ModifyFieldType.V2 where

import Data.Function ((&))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "ModifyFieldType" (\(req :: ModifyFieldTypeType) -> pure req)
  ]

data ModifyFieldTypeType = ModifyFieldTypeType
  { field :: Text
  }
  deriving (Generic)

instance Wire.Wire ModifyFieldTypeType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- The server expects an entirely different type 'Text' from the 'ModifyFieldType' endpoint than the one the generated client code sends.
-- data Text
--
-- Maybe you're trying to change the type accepted by and endpoint? If so, the following steps allow you to do so safely:
--
-- 1. Create an entirely new endpoint that's like the old one, except it accepts your new type.
-- 2. Change the client to only use the new endpoint.
-- 3. Make sure changes from step 1 and 2 are deployed.
-- 4. Delete the old endpoint.
--
-- The server returns an entirely different type 'Text' from the 'ModifyFieldType' endpoint than the one the generated client code expects.
-- data Text
--
-- Maybe you're trying to change the type returned by and endpoint? If so, the following steps allow you to do so safely:
--
-- 1. Create an entirely new endpoint that's like the old one, except it returns your new type.
-- 2. Change the client to only use the new endpoint.
-- 3. Make sure changes from step 1 and 2 are deployed.
-- 4. Delete the old endpoint.
