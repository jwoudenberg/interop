module ExampleApis.Api (service) where

import GHC.Generics (Generic)
import qualified Interop

service :: Interop.Service IO
service =
  Interop.service
    [ Interop.Endpoint
        "math"
        ( \instruction ->
            case instruction of
              DoubleNumber (OneNumber n) -> pure (n * 2)
              MultiplyNumber (TwoNumbers x y) -> pure (x * y)
        )
    ]

data Math
  = DoubleNumber OneNumber
  | MultiplyNumber TwoNumbers
  deriving (Generic)

instance Interop.Wire Math

newtype OneNumber = OneNumber {n :: Int} deriving (Generic)

instance Interop.Wire OneNumber

data TwoNumbers = TwoNumbers {x :: Int, y :: Int} deriving (Generic)

instance Interop.Wire TwoNumbers

-- generated-ruby
--
-- require "json"
-- require "net/http"
-- require "uri"
-- require "sorbet-runtime"
--
-- module Api
--
--   extend T::Sig
--   extend T::Helpers
--
--   module Math
--     sealed!
--
--     class MultiplyNumber < T::Struct
--       include Math
--
--       prop :y, Integer
--       prop :x, Integer
--     end
--
--     class DoubleNumber < T::Struct
--       include Math
--
--       prop :n, Integer
--     end
--   end
--
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
--   sig { params(body: Math).returns(Integer) }
--   def math(body:)
--     req = Net::HTTP::Post.new(@origin)
--     req["Content-Type"] = "application/json"
--
--     @http.request(req, body)
--   end
-- end
