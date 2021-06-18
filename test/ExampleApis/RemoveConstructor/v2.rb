require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module RemoveConstructor
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      module RemoveConstructorType
        extend T::Sig
        extend T::Helpers
        sealed!
        
        class AlsoKeepThisConstructor < T::Struct; include V2::RemoveConstructorType; end
        class KeepThisConstructor < T::Struct; include V2::RemoveConstructorType; end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          ctor_name, ctor_json = json.first
          case ctor_name
            when "AlsoKeepThisConstructor"
              AlsoKeepThisConstructor.from_h(ctor_json)
            when "KeepThisConstructor"
              KeepThisConstructor.from_h(ctor_json)
          end
        end
      end
      
      class RemoveConstructorType::AlsoKeepThisConstructor
        extend T::Sig
        extend T::Helpers
        
        
        
        sig { returns(Hash) }
        def to_h
          Hash["AlsoKeepThisConstructor", {
            
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            
          )
        end
      end
      
      class RemoveConstructorType::KeepThisConstructor
        extend T::Sig
        extend T::Helpers
        
        
        
        sig { returns(Hash) }
        def to_h
          Hash["KeepThisConstructor", {
            
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
      
      sig { params(arg: RemoveConstructorType).returns(RemoveConstructorType) }
      def remove_constructor(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["RemoveConstructor", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        RemoveConstructorType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"RemoveConstructor":{"requestType":{"tag":"NestedCustomType","contents":"RemoveConstructorType"},"responseType":{"tag":"NestedCustomType","contents":"RemoveConstructorType"}}},"customTypes":{"RemoveConstructorType":{"subTypes":{"Right":[{"constructorName":"KeepThisConstructor","fields":[]},{"constructorName":"AlsoKeepThisConstructor","fields":[]}]},"typeName":"RemoveConstructorType"}}}