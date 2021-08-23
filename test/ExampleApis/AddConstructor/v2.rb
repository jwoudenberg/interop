require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddConstructor
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      module AddConstructorType
        extend T::Sig
        extend T::Helpers
        sealed!
        
        class SecondConstructor < T::Struct; include V2::AddConstructorType; end
        class ThirdConstructor < T::Struct; include V2::AddConstructorType; end
        class FirstConstructor < T::Struct; include V2::AddConstructorType; end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          ctor_name, ctor_json = json.first
          case ctor_name
            when "SecondConstructor"
              SecondConstructor.from_h(ctor_json)
            when "ThirdConstructor"
              ThirdConstructor.from_h(ctor_json)
            when "FirstConstructor"
              FirstConstructor.from_h(ctor_json)
          end
        end
      end
      
      class AddConstructorType::SecondConstructor
        extend T::Sig
        extend T::Helpers
        
        
        
        sig { returns(Hash) }
        def to_h
          Hash["SecondConstructor", {
            
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            
          )
        end
      end
      
      class AddConstructorType::ThirdConstructor
        extend T::Sig
        extend T::Helpers
        
        
        
        sig { returns(Hash) }
        def to_h
          Hash["ThirdConstructor", {
            
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            
          )
        end
      end
      
      class AddConstructorType::FirstConstructor
        extend T::Sig
        extend T::Helpers
        
        
        
        sig { returns(Hash) }
        def to_h
          Hash["FirstConstructor", {
            
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
      
      sig { params(arg: AddConstructorType, headers: T::Hash[String, String]).returns(AddConstructorType) }
      def add_constructor(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["AddConstructor", arg.to_h]
        ensure_connection_started
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddConstructorType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddConstructor":{"requestType":{"tag":"NestedCustomType","contents":"AddConstructorType"},"responseType":{"tag":"NestedCustomType","contents":"AddConstructorType"}}},"customTypes":{"AddConstructorType":{"subTypes":{"Right":[{"constructorName":"FirstConstructor","fields":[]},{"constructorName":"SecondConstructor","fields":[]},{"constructorName":"ThirdConstructor","fields":[]}]},"typeName":"AddConstructorType"}}}