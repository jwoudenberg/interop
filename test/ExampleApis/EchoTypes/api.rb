require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module EchoTypes
    class Api
      
      extend T::Sig
      extend T::Helpers
      
      class Record < T::Struct; end
      
      module CustomType
        extend T::Sig
        extend T::Helpers
        sealed!
        
        class OtherConstructor < T::Struct; include Api::CustomType; end
        class Constructor < T::Struct; include Api::CustomType; end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          ctor_name, ctor_json = json.first
          case ctor_name
            when "OtherConstructor"
              OtherConstructor.from_h(ctor_json)
            when "Constructor"
              Constructor.from_h(ctor_json)
          end
        end
      end
      
      class Record
        extend T::Sig
        extend T::Helpers
        
        prop :int, Integer
        prop :text, String
        
        sig { returns(Hash) }
        def to_h
          {
            "int": int,
            "text": text,
          }
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            int: json["int"],
            text: json["text"],
          )
        end
      end
      
      class CustomType::OtherConstructor
        extend T::Sig
        extend T::Helpers
        
        
        
        sig { returns(Hash) }
        def to_h
          Hash["OtherConstructor", {
            
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            
          )
        end
      end
      
      class CustomType::Constructor
        extend T::Sig
        extend T::Helpers
        
        
        
        sig { returns(Hash) }
        def to_h
          Hash["Constructor", {
            
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            
          )
        end
      end
      
      def initialize(origin, timeout = nil)
        @origin = URI(origin)
        @http = Net::HTTP.new(@origin.host, @origin.port)
        
        unless timeout.nil?
          @http.open_timeout = timeout
          @http.read_timeout = timeout
        end
        @http.use_ssl = @origin.scheme == 'https'
      end
      
      sig { params(arg: CustomType).returns(CustomType) }
      def echo_custom_type(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo_custom_type", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        CustomType.from_h(json)
      end
      
      sig { params(arg: T::Hash[Integer, String]).returns(T::Hash[Integer, String]) }
      def echo_dict(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo_dict", arg.map { |key, val| [key, val] }]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        (json || []).map { |key, val| [key, val] }.to_h
      end
      
      sig { params(arg: Float).returns(Float) }
      def echo_float(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo_float", arg]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        json
      end
      
      sig { params(arg: Integer).returns(Integer) }
      def echo_int(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo_int", arg]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        json
      end
      
      sig { params(arg: T::Array[Integer]).returns(T::Array[Integer]) }
      def echo_list(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo_list", arg.map { |elem| elem }]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        (json || []).map { |elem| elem }
      end
      
      sig { params(arg: T.nilable(Integer)).returns(T.nilable(Integer)) }
      def echo_maybe(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo_maybe", if arg.nil? then {} else arg end]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        json && json
      end
      
      sig { params(arg: Record).returns(Record) }
      def echo_record(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo_record", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        Record.from_h(json)
      end
      
      sig { params(arg: String).returns(String) }
      def echo_text(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo_text", arg]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        json
      end
      
      sig { params(arg: NilClass).returns(NilClass) }
      def echo_unit(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo_unit", []]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        nil
      end
      
      sig { params(arg: T::Boolean).returns(T::Boolean) }
      def echo_boolean(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo_boolean", arg]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        json
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"echo_dict":{"requestType":{"tag":"Dict","contents":[{"tag":"Int"},{"tag":"Text"}]},"responseType":{"tag":"Dict","contents":[{"tag":"Int"},{"tag":"Text"}]}},"echo_custom_type":{"requestType":{"tag":"NestedCustomType","contents":"CustomType"},"responseType":{"tag":"NestedCustomType","contents":"CustomType"}},"echo_boolean":{"requestType":{"tag":"Bool"},"responseType":{"tag":"Bool"}},"echo_int":{"requestType":{"tag":"Int"},"responseType":{"tag":"Int"}},"echo_record":{"requestType":{"tag":"NestedCustomType","contents":"Record"},"responseType":{"tag":"NestedCustomType","contents":"Record"}},"echo_list":{"requestType":{"tag":"List","contents":{"tag":"Int"}},"responseType":{"tag":"List","contents":{"tag":"Int"}}},"echo_maybe":{"requestType":{"tag":"Optional","contents":{"tag":"Int"}},"responseType":{"tag":"Optional","contents":{"tag":"Int"}}},"echo_unit":{"requestType":{"tag":"Unit"},"responseType":{"tag":"Unit"}},"echo_text":{"requestType":{"tag":"Text"},"responseType":{"tag":"Text"}},"echo_float":{"requestType":{"tag":"Float"},"responseType":{"tag":"Float"}}},"customTypes":{"Record":{"subTypes":{"Left":[{"fieldType":{"tag":"Text"},"fieldName":"text"},{"fieldType":{"tag":"Int"},"fieldName":"int"}]},"typeName":"Record"},"CustomType":{"subTypes":{"Right":[{"constructorName":"Constructor","fields":[]},{"constructorName":"OtherConstructor","fields":[]}]},"typeName":"CustomType"}}}