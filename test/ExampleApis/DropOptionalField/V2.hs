{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.DropOptionalField.V2 where

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
  [ Interop.endpoint "DropOptionalField" (\(req :: DropOptionalFieldType) -> pure req)
  ]

data DropOptionalFieldType = DropOptionalFieldType
  { field :: Int
  }
  deriving (Generic)

instance Wire.Wire DropOptionalFieldType

-- Warnings when V1 is used by a server and V2 by a client:
--
-- No warnings.
