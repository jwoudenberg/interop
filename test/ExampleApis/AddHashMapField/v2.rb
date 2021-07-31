require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddHashMapField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddHashMapFieldType < T::Struct; end
      
      class AddHashMapFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :other_hash_map_field, T::Hash[Integer, Float]
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "otherHashMapField": other_hash_map_field.map { |key, val| [key, val] },
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            other_hash_map_field: (json["otherHashMapField"] || []).map { |key, val| [key, val] }.to_h,
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
      
      sig { params(arg: AddHashMapFieldType, headers: T::Hash[String, String]).returns(AddHashMapFieldType) }
      def add_hash_map_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["AddHashMapField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddHashMapFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddHashMapField":{"requestType":{"tag":"NestedCustomType","contents":"AddHashMapFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddHashMapFieldType"}}},"customTypes":{"AddHashMapFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"Dict","contents":[{"tag":"Int"},{"tag":"Float"}]},"fieldName":"otherHashMapField"}]},"typeName":"AddHashMapFieldType"}}}