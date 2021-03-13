module TestTypes.V2.DropOptionalField where

import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic)

instance Wire.Wire TestType

data Record = Record
  { field :: Int
  }
  deriving (Generic)

instance Wire.Wire Record

-- No warnings.
