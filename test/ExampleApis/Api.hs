module ExampleApis.Api (endpoints, service) where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.Endpoint
      "get_person_by_id"
      (\id' -> pure (Map.lookup id' people)),
    Interop.Endpoint
      "get_all_people"
      (\() -> pure (Map.elems people))
  ]

people :: Map.Map Int Person
people = Map.singleton 42 (Person "Jasper" "Woudenberg" [BoardGames])

data Person = Person
  { firstName :: Text,
    lastName :: Text,
    hobbies :: [Hobby]
  }
  deriving (Generic)

instance Interop.Wire Person

data Hobby
  = BoardGames
  | Piano
  | Football
  deriving (Generic)

instance Interop.Wire Hobby
