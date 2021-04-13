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
