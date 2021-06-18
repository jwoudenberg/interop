module ExampleApis.AddConstructor.V2 where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "AddConstructor" (\(_ :: AddConstructorType) -> (Proxy :: Proxy AddConstructorType))
    ]
    & either (error . show) id

data AddConstructorType
  = FirstConstructor
  | SecondConstructor
  | ThirdConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire AddConstructorType

-- Warnings for this change from Base type:
--
-- A constructor was added to a type used in responses.
-- data AddConstructorType = ThirdConstructor
--
-- Using this constructor in responses will cause failures in versions of clients that do not support it yet. Make sure to upgrade those clients before using the new constructor!
