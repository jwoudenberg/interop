module ExampleTypeChanges.V2.DropNonOptionalField where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "echo" (\(_ :: TestType) -> (Proxy :: Proxy TestType))
    ]
    & either (error . show) id

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic)

instance Wire.Wire TestType

data Record = Record
  { optionalField :: Maybe Int,
    listField :: [Int]
  }
  deriving (Generic)

instance Wire.Wire Record

-- Warnings for this change from Base type:
--
-- data TestType = OneConstructor { field }
--
-- A non-optional field was removed from a response type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First make this field optional but keep setting it on all responses. Then update clients to support the absence of the field. Finally remove the field.
