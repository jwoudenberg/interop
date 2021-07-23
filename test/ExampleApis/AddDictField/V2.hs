module ExampleApis.AddDictField.V2 where

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
  [ Interop.endpoint "AddDictField" (\(req :: AddDictFieldType) -> pure req)
  ]

data AddDictFieldType = AddDictFieldType
  { field :: Int,
    otherDictField :: Map.Map Int Float
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddDictFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
