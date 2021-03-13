module TypeChangeExamples.V2.DropNonOptionalField where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic)

instance Wire.Wire TestType

data Record = Record
  { optionalField :: Maybe Int
  }
  deriving (Generic)

instance Wire.Wire Record

-- In field: field, In constructor: OneConstructor, In type: TestType,
-- Error: A non-optional field was removed from a response type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First make this field optional but keep setting it on all responses. Then update clients to support the absence of the field. Finally remove the field.
