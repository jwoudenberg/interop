{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleApis.MakeResponseOptional.V2 where

import Data.Function ((&))
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "MakeResponseOptional" (\(req :: Int) -> pure (Just req))
  ]

-- Warnings when V2 is used by a server and V1 by a client:
--
-- Generated client code always expects types 'Maybe Int' on responses, but server considers it optional.
-- data Maybe Int
--
-- Maybe you're trying to make a type optional? If so, the following steps allow you to do so safely:
--
-- 1. Make the type optional by wrapping it in a 'Maybe', but don't return 'Nothing' values from the server yet.
-- 2. Change the client to make it support responses that omit the type.
-- 3. Make sure the changes from step 1 and 2 are deployed.
-- 4. You can now start returning 'Nothing' values from the server!
--
-- If you're currently at step 1 then this warning is expected.
