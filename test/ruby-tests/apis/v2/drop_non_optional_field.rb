require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module V2
    class DropNonOptionalField
      
      extend T::Sig
      extend T::Helpers
      
      module TestType
        extend T::Sig
        extend T::Helpers
        sealed!
        
        class OtherConstructor < T::Struct; include DropNonOptionalField::TestType; end
        class OneConstructor < T::Struct; include DropNonOptionalField::TestType; end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          ctor_name, ctor_json = json.first
          case ctor_name
            when "OtherConstructor"
              OtherConstructor.from_h(ctor_json)
            when "OneConstructor"
              OneConstructor.from_h(ctor_json)
          end
        end
      end
      
      class TestType::OtherConstructor
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
      
      class TestType::OneConstructor
        extend T::Sig
        extend T::Helpers
        
        prop :list_field, T::Array[Integer]
        prop :optional_field, T.nilable(Integer)
        
        sig { returns(Hash) }
        def to_h
          Hash["OneConstructor", {
            "listField": list_field.map { |elem| elem },
            "optionalField": if optional_field.nil? then {} else optional_field end,
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            list_field: (json["listField"] || []).map { |elem| elem },
            optional_field: json["optionalField"] && json["optionalField"],
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
      
      sig { params(arg: TestType).returns(TestType) }
      def echo(arg)
        req = Net::HTTP::Post.new(@origin)
        req["Content-Type"] = "application/json"
        
        body = ["echo", arg.to_h]
        res = @http.request(req, body.to_json)
        json = JSON.parse(res.body)
        TestType.from_h(json)
      end
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"echo":{"requestType":{"tag":"NestedCustomType","contents":"TestType"},"responseType":{"tag":"NestedCustomType","contents":"TestType"}}},"customTypes":{"TestType":{"subTypes":{"Right":[{"constructorName":"OneConstructor","fields":[{"fieldType":{"tag":"Optional","contents":{"tag":"Int"}},"fieldName":"optionalField"},{"fieldType":{"tag":"List","contents":{"tag":"Int"}},"fieldName":"listField"}]},{"constructorName":"OtherConstructor","fields":[]}]},"typeName":"TestType"}}}