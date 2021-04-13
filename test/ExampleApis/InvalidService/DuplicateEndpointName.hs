module ExampleApis.InvalidService.DuplicateEndpointName (endpoints) where

import qualified ExampleApis.Api
import qualified Interop

endpoints :: [Interop.Endpoint IO]
endpoints =
  [Interop.endpoint "get_all_people" (\() -> pure ())]
    <> ExampleApis.Api.endpoints
