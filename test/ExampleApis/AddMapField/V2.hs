module ExampleApis.AddMapField.V2 where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddMapField" (\(req :: AddMapFieldType) -> pure req)
  ]

data AddMapFieldType = AddMapFieldType
  { field :: Int,
    otherMapField :: Map.Map Int Float
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddMapFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
