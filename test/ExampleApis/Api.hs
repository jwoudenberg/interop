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
--   module Result
--     sealed!
--
--     class CannotDivideByZero < T::Struct
--       include Result
--
--
--
--       sig { returns(Hash) }
--       def to_h
--         Hash[CannotDivideByZero, serialize]
--       end
--
--       sig { params(json: Hash).returns(T.self_type) }
--       def self.from_h(json)
--         new(json)
--       end
--     end
--
--     class Result < T::Struct
--       include Result
--
--       prop :n, Integer
--
--       sig { returns(Hash) }
--       def to_h
--         Hash[Result, serialize]
--       end
--
--       sig { params(json: Hash).returns(T.self_type) }
--       def self.from_h(json)
--         new(json)
--       end
--     end
--   end
--
--   module Math
--     sealed!
--
--     class MultiplyNumber < T::Struct
--       include Math
--
--       prop :y, Integer
--       prop :x, Integer
--
--       sig { returns(Hash) }
--       def to_h
--         Hash[MultiplyNumber, serialize]
--       end
--
--       sig { params(json: Hash).returns(T.self_type) }
--       def self.from_h(json)
--         new(json)
--       end
--     end
--
--     class DoubleNumber < T::Struct
--       include Math
--
--       prop :n, Integer
--
--       sig { returns(Hash) }
--       def to_h
--         Hash[DoubleNumber, serialize]
--       end
--
--       sig { params(json: Hash).returns(T.self_type) }
--       def self.from_h(json)
--         new(json)
--       end
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
