module ExampleApis.AddOptionalField.V2 where

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
  [ Interop.endpoint "AddOptionalField" (\(req :: AddOptionalFieldType) -> pure req)
  ]

data AddOptionalFieldType = AddOptionalFieldType
  { field :: Int,
    newOptionalField :: Maybe Int
  }
  deriving (Generic)

instance Wire.Wire AddOptionalFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
