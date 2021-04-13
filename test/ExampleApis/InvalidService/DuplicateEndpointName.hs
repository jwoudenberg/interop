module ExampleApis.InvalidService.DuplicateEndpointName (endpoints) where

import qualified ExampleApis.Api
import qualified Interop

endpoints :: [Interop.Endpoint IO]
endpoints =
  [Interop.endpoint "get_all_people" (\() -> pure ())]
    <> ExampleApis.Api.endpoints

-- Interop.service fails with:
--
-- This service contains two endpoints with the same name:
--
--   get_all_people   test/ExampleApis/InvalidService/DuplicateEndpointName.hs:8
--   get_all_people   test/ExampleApis/Api.hs:19
--
-- Try renaming one of the endpoints.
--
-- If two endpoints have the same name then when I receive a request
-- I won't know which endpoint should handle it.
