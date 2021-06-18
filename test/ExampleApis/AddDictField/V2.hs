module ExampleApis.AddDictField.V2 where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "AddDictField" (\(_ :: AddDictFieldType) -> (Proxy :: Proxy AddDictFieldType))
    ]
    & either (error . show) id

data AddDictFieldType = AddDictFieldType
  { field :: Int,
    otherDictField :: Map.Map Int Float
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddDictFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
