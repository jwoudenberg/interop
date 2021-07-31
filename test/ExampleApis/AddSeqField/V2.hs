module ExampleApis.AddSeqField.V2 where

import Data.Function ((&))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "AddSeqField" (\(req :: AddSeqFieldType) -> pure req)
  ]

data AddSeqFieldType = AddSeqFieldType
  { field :: Int,
    otherSeqField :: Seq.Seq Int
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire AddSeqFieldType

-- Warnings for this change from Base type:
--
-- No warnings.
