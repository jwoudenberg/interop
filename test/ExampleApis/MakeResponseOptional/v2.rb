require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module MakeResponseOptional
    class V2
      
      extend T::Sig
      extend T::Helpers
      
      
      
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
      
      sig { params(arg: Integer, headers: T::Hash[String, String]).returns(T.nilable(Integer)) }
      def make_response_optional(arg, headers: {})
        req = Net::HTTP::Post.new(@origin, @headers.merge(headers))
        req["Content-Type"] = "application/json"
        
        body = ["MakeResponseOptional", arg]
        ensure_connection_started
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        json && json
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"MakeResponseOptional":{"requestType":{"tag":"Int"},"responseType":{"tag":"Optional","contents":{"tag":"Int"}}}},"customTypes":{}}