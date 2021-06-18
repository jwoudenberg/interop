module ExampleApis.AddOptionalField.V2 where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "AddOptionalField" (\(_ :: AddOptionalFieldType) -> (Proxy :: Proxy AddOptionalFieldType))
    ]
    & either (error . show) id

data AddOptionalFieldType = AddOptionalFieldType
  { field :: Int,
    newOptionalField :: Maybe Int
  }
  deriving (Generic)

instance Wire.Wire AddOptionalFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
