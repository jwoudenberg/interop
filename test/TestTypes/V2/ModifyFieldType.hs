module TestTypes.V2.ModifyFieldType where

import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Interop.Wire as Wire

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic)

instance Wire.Wire TestType

data Record = Record
  { field :: Text,
    optionalField :: Maybe Int
  }
  deriving (Generic)

instance Wire.Wire Record
