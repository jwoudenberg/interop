require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddFirstFieldToSecondConstructor
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      module AddFirstFieldToSecondConstructorType
        extend T::Sig
        extend T::Helpers
        sealed!
        
        class AddFirstFieldSecondConstructor < T::Struct; include V2::AddFirstFieldToSecondConstructorType; end
        class AddFirstFieldFirstConstructor < T::Struct; include V2::AddFirstFieldToSecondConstructorType; end
        
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
      
      class AddFirstFieldToSecondConstructorType::AddFirstFieldSecondConstructor
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
      
      class AddFirstFieldToSecondConstructorType::AddFirstFieldFirstConstructor
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
      
      def initialize(origin, headers: {}, timeout: nil, keep_alive_timeout: nil)
        @origin = URI(origin)
        @headers = headers
        @http = Net::HTTP.new(@origin.host, @origin.port)
        @http.use_ssl = @origin.scheme == 'https'
        
        unless timeout.nil?
          @http.open_timeout = timeout
          @http.read_timeout = timeout
        end
        
        unless keep_alive_timeout.nil?
          @http.keep_alive_timeout = keep_alive_timeout
        end
      end
      
      # Call before making requests, to ensure the first request starts an http
      # connection that subsequent requests can reuse.
      def ensure_connection_started
        @http.start unless @http.started?
      end
      
      sig { params(arg: AddFirstFieldToSecondConstructorType, headers: T::Hash[String, String]).returns(AddFirstFieldToSecondConstructorType) }
      def add_first_field_to_second_constructor(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["AddFirstFieldToSecondConstructor", arg.to_h]
        ensure_connection_started
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddFirstFieldToSecondConstructorType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddFirstFieldToSecondConstructor":{"requestType":{"tag":"NestedCustomType","contents":"AddFirstFieldToSecondConstructorType"},"responseType":{"tag":"NestedCustomType","contents":"AddFirstFieldToSecondConstructorType"}}},"customTypes":{"AddFirstFieldToSecondConstructorType":{"subTypes":{"Right":[{"constructorName":"AddFirstFieldFirstConstructor","fields":[]},{"constructorName":"AddFirstFieldSecondConstructor","fields":[{"fieldType":{"tag":"Optional","contents":{"tag":"Int"}},"fieldName":"newField"}]}]},"typeName":"AddFirstFieldToSecondConstructorType"}}}