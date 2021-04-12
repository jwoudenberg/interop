module ExampleTypeChanges.V2.ModifyFieldType where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.Endpoint "echo" (\(_ :: TestType) -> (Proxy :: Proxy TestType))
    ]
    & either (error . show) id

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic)

instance Wire.Wire TestType

data Record = Record
  { field :: Text,
    optionalField :: Maybe Int,
    listField :: [Int]
  }
  deriving (Generic)

instance Wire.Wire Record

-- Warnings for this change from Base type:
--
-- In endpoint: echo, in type: TestType, in constructor: OneConstructor, in field: field, in type: Int
-- Error: We're expecting an entirely different request type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used.
--
-- In endpoint: echo, in type: TestType, in constructor: OneConstructor, in field: field, in type: Int
-- Error: We're returning an entirely different response type. This will break old versions of clients. Consider making this change in a couple of steps to avoid failures: First, add a new endpoint using the new type. Then migrate clients over to use the new endpoint. Finally remove the old endpoint when it is no longer used.
