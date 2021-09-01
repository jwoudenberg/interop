{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.MakeRequestNonOptional.V2 where

import Data.Function ((&))
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "MakeRequestNonOptional" (\(_ :: Int) -> pure ())
  ]

-- Warnings when V2 is used by a server and V1 by a client:
--
-- The server always expects types 'Int' on requests, but the generated client code considers it optional.
-- data Int
--
-- Maybe you're trying to make a type non-optional? If so, the following steps allow you to do so safely:
--
-- 1. Change the client to always send values for the optional type.
-- 2. Make sure the changes from step 1 are deployed.
-- 3. Make the type non-optional in the server code.
--
-- If you're currently at step 3 then this warning is expected.
