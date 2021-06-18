module ExampleApis.AddEndpoint.V2 where

import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import qualified Interop

service :: Interop.Service Proxy
service =
  Interop.service
    [ Interop.endpoint "FirstEndpoint" (\(req :: Int) -> pure req),
      Interop.endpoint "SecondEndpoint" (\(_ :: Int) -> (Proxy :: Proxy Int))
    ]
    & either (error . show) id

-- Warnings for this change from Base type:
--
-- The client supports an endpoint that the server doesn't. Maybe the endpoint was recently removed from the server. If client code calls the endpoint the server will return an error.
