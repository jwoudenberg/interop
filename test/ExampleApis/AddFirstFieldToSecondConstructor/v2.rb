require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddFirstFieldToSecondConstructor
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      module AddFirstFieldType
        extend T::Sig
        extend T::Helpers
        sealed!
        
        class AddFirstFieldSecondConstructor < T::Struct; include V2::AddFirstFieldType; end
        class AddFirstFieldFirstConstructor < T::Struct; include V2::AddFirstFieldType; end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          ctor_name, ctor_json = json.first
          case ctor_name
            when "AddFirstFieldSecondConstructor"
              AddFirstFieldSecondConstructor.from_h(ctor_json)
            when "AddFirstFieldFirstConstructor"
              AddFirstFieldFirstConstructor.from_h(ctor_json)
          end
        end
      end
      
      class AddFirstFieldType::AddFirstFieldSecondConstructor
        extend T::Sig
        extend T::Helpers
        
        prop :new_field, T.nilable(Integer)
        
        sig { returns(Hash) }
        def to_h
          Hash["AddFirstFieldSecondConstructor", {
            "newField": if new_field.nil? then {} else new_field end,
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            new_field: json["newField"] && json["newField"],
          )
        end
      end
      
      class AddFirstFieldType::AddFirstFieldFirstConstructor
        extend T::Sig
        extend T::Helpers
        
        
        
        sig { returns(Hash) }
        def to_h
          Hash["AddFirstFieldFirstConstructor", {
            
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            
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
# INTEROP-SPEC:{"endpoints":{"AddFirstField":{"requestType":{"tag":"NestedCustomType","contents":"AddFirstFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddFirstFieldType"}}},"customTypes":{"AddFirstFieldType":{"subTypes":{"Right":[{"constructorName":"AddFirstFieldFirstConstructor","fields":[]},{"constructorName":"AddFirstFieldSecondConstructor","fields":[{"fieldType":{"tag":"Optional","contents":{"tag":"Int"}},"fieldName":"newField"}]}]},"typeName":"AddFirstFieldType"}}}