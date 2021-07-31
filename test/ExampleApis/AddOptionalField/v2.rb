require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddOptionalField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddOptionalFieldType < T::Struct; end
      
      class AddOptionalFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :new_optional_field, T.nilable(Integer)
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "newOptionalField": if new_optional_field.nil? then {} else new_optional_field end,
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            new_optional_field: json["newOptionalField"] && json["newOptionalField"],
            field: json["field"],
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
      
      sig { params(arg: AddOptionalFieldType, headers: T::Hash[String, String]).returns(AddOptionalFieldType) }
      def add_optional_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["AddOptionalField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddOptionalFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddOptionalField":{"requestType":{"tag":"NestedCustomType","contents":"AddOptionalFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddOptionalFieldType"}}},"customTypes":{"AddOptionalFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"Optional","contents":{"tag":"Int"}},"fieldName":"newOptionalField"}]},"typeName":"AddOptionalFieldType"}}}