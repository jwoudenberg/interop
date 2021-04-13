module ExampleTypeChanges.V2.AddEndpoint where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import qualified Interop
import qualified Interop.Wire as Wire

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "echo" (\(_ :: TestType) -> (Proxy :: Proxy TestType)),
      Interop.endpoint "new-endpoint" (\(_ :: TestType) -> (Proxy :: Proxy TestType))
    ]
    & either (error . show) id

data TestType
  = OneConstructor Record
  | OtherConstructor
  deriving (Generic, Eq, Show)

instance Wire.Wire TestType

data Record = Record
  { field :: Int,
    optionalField :: Maybe Int,
    listField :: [Int]
  }
  deriving (Generic, Eq, Show)

instance Wire.Wire Record

-- Warnings for this change from Base type:
--
-- The client supports an endpoint that the server doesn't. Maybe the endpoint was recently removed from the server. If client code calls the endpoint the server will return an error.
