require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddUnitField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddUnitFieldType < T::Struct; end
      
      class AddUnitFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :new_unit_field, NilClass
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "newUnitField": [],
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            new_unit_field: nil,
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
      
      sig { params(arg: AddUnitFieldType, headers: T::Hash[String, String]).returns(AddUnitFieldType) }
      def add_unit_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["AddUnitField", arg.to_h]
        ensure_connection_started
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddUnitFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddUnitField":{"requestType":{"tag":"NestedCustomType","contents":"AddUnitFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddUnitFieldType"}}},"customTypes":{"AddUnitFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"Unit"},"fieldName":"newUnitField"}]},"typeName":"AddUnitFieldType"}}}