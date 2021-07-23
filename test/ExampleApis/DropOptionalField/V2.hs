module ExampleApis.DropOptionalField.V2 where

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
  [ Interop.endpoint "DropOptionalField" (\(req :: DropOptionalFieldType) -> pure req)
  ]

data DropOptionalFieldType = DropOptionalFieldType
  { field :: Int
  }
  deriving (Generic)

instance Wire.Wire DropOptionalFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
