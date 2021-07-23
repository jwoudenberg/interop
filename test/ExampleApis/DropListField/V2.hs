module ExampleApis.DropListField.V2 where

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
  [ Interop.endpoint "DropListField" (\(req :: DropListFieldType) -> pure req)
  ]

data DropListFieldType = DropListFieldType
  { field :: Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire DropListFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
