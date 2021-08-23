require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddListField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddListFieldType < T::Struct; end
      
      class AddListFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :other_list_field, T::Array[Integer]
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "otherListField": other_list_field.map { |elem| elem },
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            other_list_field: (json["otherListField"] || []).map { |elem| elem },
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
      
      sig { params(arg: AddListFieldType, headers: T::Hash[String, String]).returns(AddListFieldType) }
      def add_list_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["AddListField", arg.to_h]
        ensure_connection_started
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddListFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddListField":{"requestType":{"tag":"NestedCustomType","contents":"AddListFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddListFieldType"}}},"customTypes":{"AddListFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"List","contents":{"tag":"Int"}},"fieldName":"otherListField"}]},"typeName":"AddListFieldType"}}}