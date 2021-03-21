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
              DoubleNumber (OneNumber n) -> pure (Result (OneNumber (n * 2)))
              MultiplyNumber (TwoNumbers x y) -> pure (Result (OneNumber (x * y)))
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

data Result
  = Result OneNumber
  | CannotDivideByZero
  deriving (Generic)

instance Interop.Wire Result

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
--   module JsonSerialization
--     sig { returns(String) }
--     def to_json
--       Hash[class.class_name, serialize].to_json
--     end
--
--     sig { params(json: String).returns(T.self_type) }
--     def self.from_json(json)
--       parsed = JSON.parse(json)
--       klass = "#{class_name}::#{parsed[parsed.keys.first]}".constantize
--       klass.new(parsed)
--     end
--
--     sig { returns(String) }
--     def self.class_name
--       to_s.split("::").last
--     end
--   end
--
--   module Result
--     sealed!
--     include JsonSerialization
--     extend JsonSerialization
--
--     class CannotDivideByZero < T::Struct
--       include Result
--
--
--     end
--
--     class Result < T::Struct
--       include Result
--
--       prop :n, Integer
--     end
--   end
--
--   module Math
--     sealed!
--     include JsonSerialization
--     extend JsonSerialization
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
--   sig { params(body: Math).returns(Result) }
--   def math(body:)
--     req = Net::HTTP::Post.new(@origin)
--     req["Content-Type"] = "application/json"
--
--     res = @http.request(req, body)
--     Result.parse_json(res.body)
--   end
-- end
