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
      
      sig { params(arg: ModifyListToOptionalFieldType, headers: T::Hash[String, String]).returns(ModifyListToOptionalFieldType) }
      def modify_list_to_optional_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["ModifyListToOptionalField", arg.to_h]
        ensure_connection_started
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        ModifyListToOptionalFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"ModifyListToOptionalField":{"requestType":{"tag":"NestedCustomType","contents":"ModifyListToOptionalFieldType"},"responseType":{"tag":"NestedCustomType","contents":"ModifyListToOptionalFieldType"}}},"customTypes":{"ModifyListToOptionalFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Optional","contents":{"tag":"Int"}},"fieldName":"listField"}]},"typeName":"ModifyListToOptionalFieldType"}}}