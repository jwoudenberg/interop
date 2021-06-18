require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module AddEndpoint
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      
      
      def initialize(origin, timeout = nil)
        @origin = URI(origin)
        @http = Net::HTTP.new(@origin.host, @origin.port)
        
        unless timeout.nil?
          @http.open_timeout = timeout
          @http.read_timeout = timeout
        end
        @http.use_ssl = @origin.scheme == 'https'
      end
      
      sig { params(arg: Integer).returns(Integer) }
      def second_endpoint(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["SecondEndpoint", arg]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        json
      end
      
      sig { params(arg: Integer).returns(Integer) }
      def first_endpoint(arg)
        req = Net::HTTP::Post.new(@origin)
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