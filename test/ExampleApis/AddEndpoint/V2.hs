module ExampleApis.AddEndpoint.V2 where

import Data.Function ((&))
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service endpoints
    & either (error . show) id

endpoints :: [Interop.Endpoint IO]
endpoints =
  [ Interop.endpoint "FirstEndpoint" (\(req :: Int) -> pure req),
    Interop.endpoint "SecondEndpoint" (\(req :: Int) -> pure req)
  ]

-- Warnings for this change from Base type:
--
-- The client supports an endpoint that the server doesn't. Maybe the endpoint was recently removed from the server. If client code calls the endpoint the server will return an error.
