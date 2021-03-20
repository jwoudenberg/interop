module ExampleApis.Api (service) where

import GHC.Generics (Generic)
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service
    [ Interop.Endpoint "double" (\(DoubleNumber n) -> pure (n * 2))
    ]

newtype DoubleNumber = DoubleNumber {n :: Int} deriving (Generic)

instance Interop.Wire DoubleNumber

-- generated-ruby
--
-- require "json"
-- require "net/http"
-- require "uri"
--
-- class Api
--   def initialize(origin, timeout = nil)
--     @origin = URI(origin)
--     @http = Net::HTTP.new(@origin.host, @origin.port)
--
--     unless timeout.nil?
--       @http.open_timeout = timeout
--       @http.read_timeout = timeout
--     end
--     @http.use_ssl = @origin.scheme == 'https'
--   end
--
--   def double(body:, authorization:)
--     req = Net::HTTP::Post.new(@origin)
--     req["Content-Type"] = "application/json"
--     req["Authorization"] = authorization
--
--     @http.request(req, body)
--   end
--
-- end
