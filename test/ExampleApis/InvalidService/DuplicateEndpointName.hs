module ExampleApis.InvalidService.DuplicateEndpointName (endpoints) where

import qualified Interop

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "your_answer" (\() -> pure False),
    Interop.endpoint "your_answer" (\() -> pure True)
  ]

-- Interop.service fails with:
--
-- This service contains two endpoints with the same name:
--
--   your_answer   test/ExampleApis/InvalidService/DuplicateEndpointName.hs:7
--   your_answer   test/ExampleApis/InvalidService/DuplicateEndpointName.hs:8
--
-- Try renaming one of the endpoints.
--
-- If two endpoints have the same name then when I receive a request
-- I won't know which endpoint should handle it.
