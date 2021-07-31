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
      
      sig { params(arg: AddHashSetFieldType, headers: T::Hash[String, String]).returns(AddHashSetFieldType) }
      def add_hash_set_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
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