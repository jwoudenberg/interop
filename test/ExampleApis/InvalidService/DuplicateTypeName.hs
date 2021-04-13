module ExampleApis.InvalidService.DuplicateTypeName (endpoints) where

import Data.Text (Text)
import qualified ExampleApis.Api
import GHC.Generics (Generic)
import qualified Interop

endpoints :: [Interop.Endpoint IO]
endpoints =
  [Interop.endpoint "random_hobby" (\() -> pure (Hobby "Boardgames"))]
    <> ExampleApis.Api.endpoints

newtype Hobby = Hobby {name :: Text}
  deriving (Generic)

instance Interop.Wire Hobby

-- Interop.service fails with:
--
-- The service uses two types with the same name:
--
--   ExampleApis.Api.Hobby
--   ExampleApis.InvalidService.DuplicateTypeName.Hobby
--
-- Try renaming one of the types.
--
-- Unique names avoid potential naming conflicts in generated client
-- code. There's many tricks I could play to avoid conflicts in
-- generated code, such as adding suffixes to dupplicate names or
-- generating multiple files if necessary. All of these make it harder
-- to understand how I work though, so I'd prefer not to.
