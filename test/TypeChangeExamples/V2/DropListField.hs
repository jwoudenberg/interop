module TypeChangeExamples.V2.DropListField where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire TestType

data Record = Record
  { field :: Int,
    optionalField :: Maybe Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire Record

-- Warnings for this change from Base type:
--
-- In field: listField, In constructor: OneConstructor, In type: TestType,
-- Error: A non-optional field was removed from a response type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First make this field optional but keep setting it on all responses. Then update clients to support the absence of the field. Finally remove the field.
