require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddSeqField
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      class AddSeqFieldType < T::Struct; end
      
      class AddSeqFieldType
        extend T::Sig
        extend T::Helpers
        
        prop :other_seq_field, T::Array[Integer]
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          {
            "otherSeqField": other_seq_field.map { |elem| elem },
            "field": field,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            other_seq_field: (json["otherSeqField"] || []).map { |elem| elem },
            field: json["field"],
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
      
      sig { params(arg: AddSeqFieldType, headers: T::Hash[String, String]).returns(AddSeqFieldType) }
      def add_seq_field(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["AddSeqField", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        AddSeqFieldType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"AddSeqField":{"requestType":{"tag":"NestedCustomType","contents":"AddSeqFieldType"},"responseType":{"tag":"NestedCustomType","contents":"AddSeqFieldType"}}},"customTypes":{"AddSeqFieldType":{"subTypes":{"Left":[{"fieldType":{"tag":"Int"},"fieldName":"field"},{"fieldType":{"tag":"List","contents":{"tag":"Int"}},"fieldName":"otherSeqField"}]},"typeName":"AddSeqFieldType"}}}