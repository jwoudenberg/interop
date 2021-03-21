module ExampleTypeChanges.V2.AddListField where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire TestType

data Record = Record
  { field :: Int,
    optionalField :: Maybe Int,
    listField :: [Int],
    otherListField :: [Int]
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire Record

-- Warnings for this change from Base type:
--
-- No warnings.
