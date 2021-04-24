module Interop.Spec
  ( spec,
    Service (..),
    Endpoint (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Interop.Service as Service
import qualified Interop.Wire as Wire
import qualified Interop.Wire.Flat as Flat

data Service = Service
  { customTypes :: Map.Map Text Flat.CustomType,
    endpoints :: Map.Map Text Endpoint
  }
  deriving (Generic)

instance Aeson.ToJSON Service

instance Aeson.FromJSON Service

data Endpoint = Endpoint
  { requestType :: Flat.Type,
    responseType :: Flat.Type
  }
  deriving (Generic)

instance Aeson.ToJSON Endpoint

instance Aeson.FromJSON Endpoint

spec :: Service.Service m -> Service
spec service =
  Service
    { customTypes = customTypeMap service,
      endpoints =
        fmap
          ( \endpoint ->
              Endpoint
                (requestTypeForEndpoint endpoint)
                (responseTypeForEndpoint endpoint)
          )
          (Service.endpoints service)
    }

requestTypeForEndpoint :: Service.Endpoint m -> Flat.Type
requestTypeForEndpoint (Service.Endpoint _ _ (_ :: req -> m res)) =
  Wire.type_ (Proxy :: Proxy req)
    & Flat.fromFieldType

responseTypeForEndpoint :: Service.Endpoint m -> Flat.Type
responseTypeForEndpoint (Service.Endpoint _ _ (_ :: req -> m res)) =
  Wire.type_ (Proxy :: Proxy res)
    & Flat.fromFieldType

customTypeMap :: Service.Service m -> Map.Map Text Flat.CustomType
customTypeMap service =
  Service.customTypes service
    & fmap (\customType -> (Flat.typeName customType, customType))
    & Map.fromList
