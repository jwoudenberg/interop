module ExampleApis.ModifyOptionalToListField.V2 where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "ModifyOptionalToListField" (\(_ :: ModifyOptionalToListFieldType) -> (Proxy :: Proxy ModifyOptionalToListFieldType))
    ]
    & either (error . show) id

data ModifyOptionalToListFieldType = ModifyOptionalToListFieldType
  { optionalField :: [Int]
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire ModifyOptionalToListFieldType

-- Warnings for this change from Base type:
--
-- A type used in requests has changed.
-- data Int
--
-- This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used.
--
-- A type used in responses has changed.
-- data Int
--
-- This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used.
