{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.ModifyListToOptionalField.V2 where

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
  [ Interop.endpoint "ModifyListToOptionalField" (\(req :: ModifyListToOptionalFieldType) -> pure req)
  ]

data ModifyListToOptionalFieldType = ModifyListToOptionalFieldType
  { listField :: Maybe Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire ModifyListToOptionalFieldType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- The server expects an entirely different type 'Int' from the 'ModifyListToOptionalField' endpoint than the one the generated client code sends.
-- data Int
--
-- Maybe you're trying to change the type accepted by and endpoint? If so, the following steps allow you to do so safely:
--
-- 1. Create an entirely new endpoint that's like the old one, except it accepts your new type.
-- 2. Change the client to only use the new endpoint.
-- 3. Make sure changes from step 1 and 2 are deployed.
-- 4. Delete the old endpoint.
--
-- The server returns an entirely different type 'Int' from the 'ModifyListToOptionalField' endpoint than the one the generated client code expects.
-- data Int
--
-- Maybe you're trying to change the type returned by and endpoint? If so, the following steps allow you to do so safely:
--
-- 1. Create an entirely new endpoint that's like the old one, except it returns your new type.
-- 2. Change the client to only use the new endpoint.
-- 3. Make sure changes from step 1 and 2 are deployed.
-- 4. Delete the old endpoint.
