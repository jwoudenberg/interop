{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddFirstField.V2 where

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
  [ Interop.endpoint "AddFirstField" (\(req :: AddFirstFieldType) -> pure req)
  ]

data AddFirstFieldType = AddFirstField {newField :: Maybe Int}
  deriving (Generic)

instance Wire.Wire AddFirstFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
