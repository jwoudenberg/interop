require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module DropAllFields
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      module DropAllFieldsType
        extend T::Sig
        extend T::Helpers
        sealed!
        
        class DropAllFieldsSecondConstructor < T::Struct; include V2::DropAllFieldsType; end
        class DropAllFieldsFirstConstructor < T::Struct; include V2::DropAllFieldsType; end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          ctor_name, ctor_json = json.first
          case ctor_name
            when "DropAllFieldsSecondConstructor"
              DropAllFieldsSecondConstructor.from_h(ctor_json)
            when "DropAllFieldsFirstConstructor"
              DropAllFieldsFirstConstructor.from_h(ctor_json)
          end
        end
      end
      
      class DropAllFieldsType::DropAllFieldsSecondConstructor
        extend T::Sig
        extend T::Helpers
        
        
        
        sig { returns(Hash) }
        def to_h
          Hash["DropAllFieldsSecondConstructor", {
            
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            
          )
        end
      end
      
      class DropAllFieldsType::DropAllFieldsFirstConstructor
        extend T::Sig
        extend T::Helpers
        
        
        
        sig { returns(Hash) }
        def to_h
          Hash["DropAllFieldsFirstConstructor", {
            
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            
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
      
      sig { params(arg: DropAllFieldsType, headers: T::Hash[String, String]).returns(DropAllFieldsType) }
      def drop_all_fields(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["DropAllFields", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        DropAllFieldsType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"DropAllFields":{"requestType":{"tag":"NestedCustomType","contents":"DropAllFieldsType"},"responseType":{"tag":"NestedCustomType","contents":"DropAllFieldsType"}}},"customTypes":{"DropAllFieldsType":{"subTypes":{"Right":[{"constructorName":"DropAllFieldsFirstConstructor","fields":[]},{"constructorName":"DropAllFieldsSecondConstructor","fields":[]}]},"typeName":"DropAllFieldsType"}}}