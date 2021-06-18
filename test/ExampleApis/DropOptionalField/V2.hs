module ExampleApis.DropOptionalField.V2 where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "DropOptionalField" (\(_ :: DropOptionalFieldType) -> (Proxy :: Proxy DropOptionalFieldType))
    ]
    & either (error . show) id

data DropOptionalFieldType = DropOptionalFieldType
  { field :: Int
  }
  deriving (Generic)

instance Wire.Wire DropOptionalFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
