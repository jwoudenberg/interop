require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module ModifyFieldType
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class ModifyFieldTypeType < T::Struct; end
      
      class ModifyFieldTypeType
        extend T::Sig
        extend T::Helpers
        
        prop :field, String
        
        sig { returns(Hash) }
        def to_h
          {
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
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
      
      sig { params(arg: ModifyFieldTypeType).returns(ModifyFieldTypeType) }
      def modify_field_type(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["ModifyFieldType", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        ModifyFieldTypeType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"ModifyFieldType":{"requestType":{"tag":"NestedCustomType","contents":"ModifyFieldTypeType"},"responseType":{"tag":"NestedCustomType","contents":"ModifyFieldTypeType"}}},"customTypes":{"ModifyFieldTypeType":{"subTypes":{"Left":[{"fieldType":{"tag":"Text"},"fieldName":"field"}]},"typeName":"ModifyFieldTypeType"}}}