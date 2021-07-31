module ExampleApis.AddSetField.V2 where

import Data.Function ((&))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddSetField" (\(req :: AddSetFieldType) -> pure req)
  ]

data AddSetFieldType = AddSetFieldType
  { field :: Int,
    otherSetField :: Set.Set Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddSetFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
