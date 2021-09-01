{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddFirstField.V2 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Interop

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

instance Interop.Wire AddFirstFieldType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- No warnings.
