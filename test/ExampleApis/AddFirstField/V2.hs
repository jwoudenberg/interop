module ExampleApis.AddFirstField.V2 where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "AddFirstField" (\(_ :: AddFirstFieldType) -> (Proxy :: Proxy AddFirstFieldType))
    ]
    & either (error . show) id

data AddFirstFieldType = AddFirstField {newField :: Maybe Int}
  deriving (Generic)

instance Wire.Wire AddFirstFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
