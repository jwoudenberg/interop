require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddSetField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddSetFieldType < T::Struct; end
      
      class AddSetFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :other_set_field, T::Array[Integer]
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "otherSetField": other_set_field.map { |elem| elem },
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            other_set_field: (json["otherSetField"] || []).map { |elem| elem },
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
      
      sig { params(arg: AddSetFieldType).returns(AddSetFieldType) }
      def add_set_field(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["AddSetField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddSetFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddSetField":{"requestType":{"tag":"NestedCustomType","contents":"AddSetFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddSetFieldType"}}},"customTypes":{"AddSetFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"List","contents":{"tag":"Int"}},"fieldName":"otherSetField"}]},"typeName":"AddSetFieldType"}}}