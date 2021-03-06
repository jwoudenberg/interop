require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddNonOptionalField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddNonOptionalFieldType < T::Struct; end
      
      class AddNonOptionalFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :new_field, Integer
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "newField": new_field,
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            new_field: json["newField"],
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
      
      sig { params(arg: AddNonOptionalFieldType).returns(AddNonOptionalFieldType) }
      def add_non_optional_field(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["AddNonOptionalField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddNonOptionalFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddNonOptionalField":{"requestType":{"tag":"NestedCustomType","contents":"AddNonOptionalFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddNonOptionalFieldType"}}},"customTypes":{"AddNonOptionalFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"Int"},"fieldName":"newField"}]},"typeName":"AddNonOptionalFieldType"}}}