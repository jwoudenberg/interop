require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddHashSetField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddHashSetFieldType < T::Struct; end
      
      class AddHashSetFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :other_hash_set_field, T::Array[Integer]
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "otherHashSetField": other_hash_set_field.map { |elem| elem },
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            other_hash_set_field: (json["otherHashSetField"] || []).map { |elem| elem },
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
      
      sig { params(arg: AddHashSetFieldType).returns(AddHashSetFieldType) }
      def add_hash_set_field(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["AddHashSetField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddHashSetFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddHashSetField":{"requestType":{"tag":"NestedCustomType","contents":"AddHashSetFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddHashSetFieldType"}}},"customTypes":{"AddHashSetFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"List","contents":{"tag":"Int"}},"fieldName":"otherHashSetField"}]},"typeName":"AddHashSetFieldType"}}}