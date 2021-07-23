module ExampleApis.DropNonOptionalField.V2 where

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
  [ Interop.endpoint "DropNonOptionalField" (\(req :: DropNonOptionalFieldType) -> pure req)
  ]

data DropNonOptionalFieldType = DropNonOptionalFieldType
  { field1 :: Int
  }
  deriving (Generic)

instance Wire.Wire DropNonOptionalFieldType

-- Warnings for this change from Base type:
--
-- A type used in responses has lost a mandatory field.
-- data DropNonOptionalFieldType { field2 }
--
-- This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First make this field optional but keep setting it on all responses. Then update clients to support the absence of the field. Finally remove the field.
