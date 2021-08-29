{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddHashMapField.V2 where

import Data.Function ((&))
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddHashMapField" (\(req :: AddHashMapFieldType) -> pure req)
  ]

data AddHashMapFieldType = AddHashMapFieldType
  { field :: Int,
    otherHashMapField :: HashMap.HashMap Int Float
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddHashMapFieldType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- No warnings.
