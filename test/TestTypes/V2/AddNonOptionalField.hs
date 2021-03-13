module TestTypes.V2.AddNonOptionalField where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic)

instance Wire.Wire TestType

data Record = Record
  { field :: Int,
    optionalField :: Maybe Int,
    newField :: Int
  }
  deriving (Generic)

instance Wire.Wire Record

-- In field: newField, In constructor: OneConstructor, In type: TestType,
-- Error: A non-optional field was added to a request type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First add an optional field. Then update clients to always set the optional field. Finally make the new field non-optional.
