module ExampleApis.RemoveConstructor.V2 where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "RemoveConstructor" (\(_ :: RemoveConstructorType) -> (Proxy :: Proxy RemoveConstructorType))
    ]
    & either (error . show) id

data RemoveConstructorType
  = KeepThisConstructor
  | AlsoKeepThisConstructor
  deriving (Generic)

instance Wire.Wire RemoveConstructorType

-- Warnings for this change from Base type:
--
-- A constructor was removed from a type used in requests.
-- data RemoveConstructorType = RemoveThisConstructor
--
-- Clients that send us requests using the removed constructor will receive an error. Before going forward with this change, make sure clients are no longer using the constructor in requests!
