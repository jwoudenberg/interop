module ExampleTypeChanges.V2.AddConstructor where

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
  | NewConstructor
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
-- data TestType = NewConstructor
--
-- Warning: A constructor was added to a response type. Using this constructor in responses will cause failures in versions of clients that do not support it yet. Make sure to upgrade those clients before using the new constructor!
