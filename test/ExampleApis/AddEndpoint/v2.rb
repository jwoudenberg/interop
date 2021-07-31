require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddEndpoint
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      
      
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
      
      sig { params(arg: Integer, headers: T::Hash[String, String]).returns(Integer) }
      def second_endpoint(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["SecondEndpoint", arg]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        json
      end
      
      sig { params(arg: Integer, headers: T::Hash[String, String]).returns(Integer) }
      def first_endpoint(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["FirstEndpoint", arg]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        json
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"SecondEndpoint":{"requestType":{"tag":"Int"},"responseType":{"tag":"Int"}},"FirstEndpoint":{"requestType":{"tag":"Int"},"responseType":{"tag":"Int"}}},"customTypes":{}}