require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddFirstField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddFirstFieldType < T::Struct; end
      
      class AddFirstFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :new_field, T.nilable(Integer)
        
        sig { returns(Hash) }
        def to_h
          {
            "newField": if new_field.nil? then {} else new_field end,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            new_field: json["newField"] && json["newField"],
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
      
      sig { params(arg: AddFirstFieldType).returns(AddFirstFieldType) }
      def add_first_field(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["AddFirstField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddFirstFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddFirstField":{"requestType":{"tag":"NestedCustomType","contents":"AddFirstFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddFirstFieldType"}}},"customTypes":{"AddFirstFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Optional","contents":{"tag":"Int"}},"fieldName":"newField"}]},"typeName":"AddFirstFieldType"}}}