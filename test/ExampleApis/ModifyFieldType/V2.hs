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

-- Warnings when V1 is used by a server and V2 by a client:
--
-- A type used in requests has changed.
-- data Int
--
-- This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used.
--
-- A type used in responses has changed.
-- data Int
--
-- This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used.
