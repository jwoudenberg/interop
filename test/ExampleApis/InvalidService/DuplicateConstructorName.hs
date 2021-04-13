module ExampleApis.InvalidService.DuplicateConstructorName (endpoints) where

import qualified ExampleApis.Api
import GHC.Generics (Generic)
import qualified Interop

endpoints :: [Interop.Endpoint IO]
endpoints =
  [Interop.endpoint "random_sport" (\() -> pure Football)]
    <> ExampleApis.Api.endpoints

data Sport
  = Football
  | Tennis
  | Hockey
  deriving (Generic)

instance Interop.Wire Sport

-- Interop.service fails with:
--
-- The service uses two constructors with the same name:
--
--   ExampleApis.Api (Hobby(Football))
--   ExampleApis.InvalidService.DuplicateConstructorName (Sport(Football))
--
-- Try renaming one of the constructors.
--
-- Unique names avoid potential naming conflicts in generated client
-- code. There's many tricks I could play to avoid conflicts in
-- generated code, such as adding suffixes to dupplicate names or
-- generating multiple files if necessary. All of these make it harder
-- to understand how I work though, so I'd prefer not to.
