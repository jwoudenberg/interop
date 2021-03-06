require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddDictField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddDictFieldType < T::Struct; end
      
      class AddDictFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :other_dict_field, T::Hash[Integer, Float]
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "otherDictField": other_dict_field.map { |key, val| [key, val] },
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            other_dict_field: (json["otherDictField"] || []).map { |key, val| [key, val] }.to_h,
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
      
      sig { params(arg: AddDictFieldType).returns(AddDictFieldType) }
      def add_dict_field(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["AddDictField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddDictFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddDictField":{"requestType":{"tag":"NestedCustomType","contents":"AddDictFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddDictFieldType"}}},"customTypes":{"AddDictFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"Dict","contents":[{"tag":"Int"},{"tag":"Float"}]},"fieldName":"otherDictField"}]},"typeName":"AddDictFieldType"}}}