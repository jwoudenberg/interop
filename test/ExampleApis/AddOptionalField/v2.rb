require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddOptionalField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddOptionalFieldType < T::Struct; end
      
      class AddOptionalFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :new_optional_field, T.nilable(Integer)
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "newOptionalField": if new_optional_field.nil? then {} else new_optional_field end,
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            new_optional_field: json["newOptionalField"] && json["newOptionalField"],
            field: json["field"],
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
      
      sig { params(arg: AddOptionalFieldType, headers: T::Hash[String, String]).returns(AddOptionalFieldType) }
      def add_optional_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["AddOptionalField", arg.to_h]
        ensure_connection_started
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddOptionalFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddOptionalField":{"requestType":{"tag":"NestedCustomType","contents":"AddOptionalFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddOptionalFieldType"}}},"customTypes":{"AddOptionalFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"Optional","contents":{"tag":"Int"}},"fieldName":"newOptionalField"}]},"typeName":"AddOptionalFieldType"}}}