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
