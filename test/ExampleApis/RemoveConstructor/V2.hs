{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.RemoveConstructor.V2 where

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
  [ Interop.endpoint "RemoveConstructor" (\(req :: RemoveConstructorType) -> pure req)
  ]

data RemoveConstructorType
  = KeepThisConstructor
  | AlsoKeepThisConstructor
  deriving (Generic)

instance Wire.Wire RemoveConstructorType

-- Warnings when V1 is used by a server and V2 by a client:
--
-- A constructor was removed from a type used in requests.
-- data RemoveConstructorType = RemoveThisConstructor
--
-- Clients that send us requests using the removed constructor will receive an error. Before going forward with this change, make sure clients are no longer using the constructor in requests!
