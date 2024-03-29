require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module ModifyOptionalToListField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class ModifyOptionalToListFieldType < T::Struct; end
      
      class ModifyOptionalToListFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :optional_field, T::Array[Integer]
        
        sig { returns(Hash) }
        def to_h
          {
            "optionalField": optional_field.map { |elem| elem },
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            optional_field: (json["optionalField"] || []).map { |elem| elem },
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
      
      sig { params(arg: ModifyOptionalToListFieldType, headers: T::Hash[String, String]).returns(ModifyOptionalToListFieldType) }
      def modify_optional_to_list_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["ModifyOptionalToListField", arg.to_h]
        ensure_connection_started
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        ModifyOptionalToListFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"ModifyOptionalToListField":{"requestType":{"tag":"NestedCustomType","contents":"ModifyOptionalToListFieldType"},"responseType":{"tag":"NestedCustomType","contents":"ModifyOptionalToListFieldType"}}},"customTypes":{"ModifyOptionalToListFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"List","contents":{"tag":"Int"}},"fieldName":"optionalField"}]},"typeName":"ModifyOptionalToListFieldType"}}}