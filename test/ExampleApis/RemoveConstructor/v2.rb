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
      
      sig { params(arg: RemoveConstructorType, headers: T::Hash[String, String]).returns(RemoveConstructorType) }
      def remove_constructor(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["RemoveConstructor", arg.to_h]
        ensure_connection_started
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        RemoveConstructorType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"RemoveConstructor":{"requestType":{"tag":"NestedCustomType","contents":"RemoveConstructorType"},"responseType":{"tag":"NestedCustomType","contents":"RemoveConstructorType"}}},"customTypes":{"RemoveConstructorType":{"subTypes":{"Right":[{"constructorName":"KeepThisConstructor","fields":[]},{"constructorName":"AlsoKeepThisConstructor","fields":[]}]},"typeName":"RemoveConstructorType"}}}