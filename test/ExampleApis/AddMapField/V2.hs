{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddMapField.V2 where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddMapField" (\(req :: AddMapFieldType) -> pure req)
  ]

data AddMapFieldType = AddMapFieldType
  { field :: Int,
    otherMapField :: Map.Map Int Float
  }
  deriving (Generic, Eq, Show)

instance Interop.Wire AddMapFieldType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- No warnings.
