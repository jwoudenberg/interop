module ExampleApis.ModifyListToOptionalField.V2 where

import Data.Function ((&))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "ModifyListToOptionalField" (\(req :: ModifyListToOptionalFieldType) -> pure req)
  ]

data ModifyListToOptionalFieldType = ModifyListToOptionalFieldType
  { listField :: Maybe Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire ModifyListToOptionalFieldType

-- Warnings for this change from Base type:
--
-- A type used in requests has changed.
-- data List Int
--
-- This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used.
--
-- A type used in responses has changed.
-- data List Int
--
-- This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used.
