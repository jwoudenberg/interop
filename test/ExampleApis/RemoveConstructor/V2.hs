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

-- Warnings when V2 is used by a server and V1 by a client:
--
-- A constructor was added to a type used in responses.
-- data RemoveConstructorType = RemoveThisConstructor
--
-- Using this constructor in responses will cause failures in versions of clients that do not support it yet. Make sure to upgrade those clients before using the new constructor!
