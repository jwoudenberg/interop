module ExampleTypeChanges.V2.RemoveConstructor where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data TestType
  = OneConstructor Record
  deriving (Generic)

instance Wire.Wire TestType

data Record = Record
  { field :: Int,
    optionalField :: Maybe Int,
    listField :: [Int]
  }
  deriving (Generic)

instance Wire.Wire Record

-- Warnings for this change from Base type:
--
-- In endpoint: fake-endpoint, in type: TestType, in constructor: OtherConstructor
-- Warning: A constructor was removed from a request type. Clients that send us requests using the removed constructor will receive an error. Before going forward with this change, make sure clients are no longer using the constructor in requests!
