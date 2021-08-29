{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.AddEndpoint.V2 where

import Data.Function ((&))
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "FirstEndpoint" (\(req :: Int) -> pure req),
    Interop.endpoint "SecondEndpoint" (\(req :: Int) -> pure req)
  ]

-- Warnings when V1 is used by a server and V2 by a client:
--
-- client uses endpoint unsupported by server
--
--
-- The client supports an endpoint that the server doesn't. Maybe the endpoint was recently removed from the server. If client code calls the endpoint the server will return an error.
