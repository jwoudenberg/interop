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
-- The generated client code supports an endpoint 'SecondEndpoint', but the server doesn't have such an endpoint.
--
--
-- Maybe you're trying to remove an endpoint? If so, make sure to follow these steps:
--
-- 1. Stop using the endpoint in your client code.
-- 2. Make sure the clients from step 1 are deployed.
-- 3. Remove the endpoint from the server code.
--
-- If you're currently at step 3 this warning is expected.
