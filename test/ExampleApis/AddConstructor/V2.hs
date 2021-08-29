{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddConstructor.V2 where

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
  [ Interop.endpoint "AddConstructor" (\(req :: AddConstructorType) -> pure req)
  ]

data AddConstructorType
  = FirstConstructor
  | SecondConstructor
  | ThirdConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire AddConstructorType

-- Warnings when V2 is used by a server and V1 by a client:
--
-- A constructor was removed from a type used in requests.
-- data AddConstructorType = ThirdConstructor
--
-- Clients that send us requests using the removed constructor will receive an error. Before going forward with this change, make sure clients are no longer using the constructor in requests!
