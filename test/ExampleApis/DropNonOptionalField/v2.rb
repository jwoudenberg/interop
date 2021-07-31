require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module DropNonOptionalField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class DropNonOptionalFieldType < T::Struct; end
      
      class DropNonOptionalFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :field1, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "field1": field1,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            field1: json["field1"],
          )
        end
      end
      
      def initialize(origin, headers: {}, timeout: nil)
        @origin = URI(origin)
        @headers = headers
        @http = Net::HTTP.new(@origin.host, @origin.port)
        
        unless timeout.nil?
          @http.open_timeout = timeout
          @http.read_timeout = timeout
        end
        @http.use_ssl = @origin.scheme == 'https'
      end
      
      sig { params(arg: DropNonOptionalFieldType, headers: T::Hash[String, String]).returns(DropNonOptionalFieldType) }
      def drop_non_optional_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["DropNonOptionalField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        DropNonOptionalFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"DropNonOptionalField":{"requestType":{"tag":"NestedCustomType","contents":"DropNonOptionalFieldType"},"responseType":{"tag":"NestedCustomType","contents":"DropNonOptionalFieldType"}}},"customTypes":{"DropNonOptionalFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field1"}]},"typeName":"DropNonOptionalFieldType"}}}