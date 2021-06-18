require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module DropOptionalField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class DropOptionalFieldType < T::Struct; end
      
      class DropOptionalFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            field: json["field"],
          )
        end
      end
      
      def initialize(origin, timeout = nil)
        @origin = URI(origin)
        @http = Net::HTTP.new(@origin.host, @origin.port)
        
        unless timeout.nil?
          @http.open_timeout = timeout
          @http.read_timeout = timeout
        end
        @http.use_ssl = @origin.scheme == 'https'
      end
      
      sig { params(arg: DropOptionalFieldType).returns(DropOptionalFieldType) }
      def drop_optional_field(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["DropOptionalField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        DropOptionalFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"DropOptionalField":{"requestType":{"tag":"NestedCustomType","contents":"DropOptionalFieldType"},"responseType":{"tag":"NestedCustomType","contents":"DropOptionalFieldType"}}},"customTypes":{"DropOptionalFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"}]},"typeName":"DropOptionalFieldType"}}}