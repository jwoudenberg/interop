require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module ModifyListToOptionalField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class ModifyListToOptionalFieldType < T::Struct; end
      
      class ModifyListToOptionalFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :list_field, T.nilable(Integer)
        
        sig { returns(Hash) }
        def to_h
          {
            "listField": if list_field.nil? then {} else list_field end,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            list_field: json["listField"] && json["listField"],
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
      
      sig { params(arg: ModifyListToOptionalFieldType).returns(ModifyListToOptionalFieldType) }
      def modify_list_to_optional_field(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["ModifyListToOptionalField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        ModifyListToOptionalFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"ModifyListToOptionalField":{"requestType":{"tag":"NestedCustomType","contents":"ModifyListToOptionalFieldType"},"responseType":{"tag":"NestedCustomType","contents":"ModifyListToOptionalFieldType"}}},"customTypes":{"ModifyListToOptionalFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Optional","contents":{"tag":"Int"}},"fieldName":"listField"}]},"typeName":"ModifyListToOptionalFieldType"}}}