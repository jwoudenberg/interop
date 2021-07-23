module ExampleApis.EchoTypes.Api (endpoints, service) where

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
  [ Interop.endpoint
      "echo_record"
      (\(req :: Record) -> pure req),
    Interop.endpoint
      "echo_custom_type"
      (\(req :: CustomType) -> pure req),
    Interop.endpoint
      "echo_boolean"
      (\(req :: Bool) -> pure req),
    Interop.endpoint
      "echo_int"
      (\(req :: Int) -> pure req),
    Interop.endpoint
      "echo_float"
      (\(req :: Float) -> pure req),
    Interop.endpoint
      "echo_text"
      (\(req :: Text) -> pure req),
    Interop.endpoint
      "echo_maybe"
      (\(req :: Maybe Int) -> pure req),
    Interop.endpoint
      "echo_unit"
      (\(req :: ()) -> pure req),
    Interop.endpoint
      "echo_dict"
      (\(req :: Map.Map Int Text) -> pure req),
    Interop.endpoint
      "echo_list"
      (\(req :: [Int]) -> pure req)
  ]

data Record = Record
  { text :: Text,
    int :: Int
  }
  deriving (Generic)

instance Interop.Wire Record

data CustomType
  = Constructor
  | OtherConstructor
  deriving (Generic)

instance Interop.Wire CustomType
