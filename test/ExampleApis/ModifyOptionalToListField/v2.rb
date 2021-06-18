require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module ModifyOptionalToListField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class ModifyOptionalToListFieldType < T::Struct; end
      
      class ModifyOptionalToListFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :optional_field, T::Array[Integer]
        
        sig { returns(Hash) }
        def to_h
          {
            "optionalField": optional_field.map { |elem| elem },
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            optional_field: (json["optionalField"] || []).map { |elem| elem },
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
      
      sig { params(arg: ModifyOptionalToListFieldType).returns(ModifyOptionalToListFieldType) }
      def modify_optional_to_list_field(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["ModifyOptionalToListField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        ModifyOptionalToListFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"ModifyOptionalToListField":{"requestType":{"tag":"NestedCustomType","contents":"ModifyOptionalToListFieldType"},"responseType":{"tag":"NestedCustomType","contents":"ModifyOptionalToListFieldType"}}},"customTypes":{"ModifyOptionalToListFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"List","contents":{"tag":"Int"}},"fieldName":"optionalField"}]},"typeName":"ModifyOptionalToListFieldType"}}}