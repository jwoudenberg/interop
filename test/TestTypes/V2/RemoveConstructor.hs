module TestTypes.V2.RemoveConstructor where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data TestType
  = OneConstructor Record
  deriving (Generic)

instance Wire.Wire TestType

data Record = Record
  { field :: Int,
    optionalField :: Maybe Int
  }
  deriving (Generic)

instance Wire.Wire Record
