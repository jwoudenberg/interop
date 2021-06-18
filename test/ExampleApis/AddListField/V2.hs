module ExampleApis.AddListField.V2 where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "AddListField" (\(_ :: AddListFieldType) -> (Proxy :: Proxy AddListFieldType))
    ]
    & either (error . show) id

data AddListFieldType = AddListFieldType
  { field :: Int,
    otherListField :: [Int]
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddListFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
