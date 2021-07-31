require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddMapField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddMapFieldType < T::Struct; end
      
      class AddMapFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :other_map_field, T::Hash[Integer, Float]
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "otherMapField": other_map_field.map { |key, val| [key, val] },
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            other_map_field: (json["otherMapField"] || []).map { |key, val| [key, val] }.to_h,
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
      
      sig { params(arg: AddMapFieldType, headers: T::Hash[String, String]).returns(AddMapFieldType) }
      def add_map_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["AddMapField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddMapFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddMapField":{"requestType":{"tag":"NestedCustomType","contents":"AddMapFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddMapFieldType"}}},"customTypes":{"AddMapFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"Dict","contents":[{"tag":"Int"},{"tag":"Float"}]},"fieldName":"otherMapField"}]},"typeName":"AddMapFieldType"}}}