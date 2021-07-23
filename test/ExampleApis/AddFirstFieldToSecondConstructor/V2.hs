module ExampleApis.AddFirstFieldToSecondConstructor.V2 where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "AddFirstFieldToSecondConstructor" (\(_ :: AddFirstFieldToSecondConstructorType) -> (Proxy :: Proxy AddFirstFieldToSecondConstructorType))
    ]
    & either (error . show) id

data AddFirstFieldToSecondConstructorType
  = AddFirstFieldFirstConstructor
  | AddFirstFieldSecondConstructor NewRecord
  deriving (Generic)

instance Wire.Wire AddFirstFieldToSecondConstructorType

data NewRecord = NewRecord
  { newField :: Maybe Int
  }
  deriving (Generic)

instance Wire.Wire NewRecord

-- Warnings for this change from Base type:
--
-- No warnings.
