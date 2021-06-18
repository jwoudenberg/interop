module ExampleApis.DropListField.V2 where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "DropListField" (\(_ :: DropListFieldType) -> (Proxy :: Proxy DropListFieldType))
    ]
    & either (error . show) id

data DropListFieldType = DropListFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire DropListFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
