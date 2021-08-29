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

-- Warnings when V2 is used by a server and V1 by a client:
--
-- No warnings.
