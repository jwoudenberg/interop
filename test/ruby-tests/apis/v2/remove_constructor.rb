require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  module V2
    class RemoveConstructor
      
      extend T::Sig
      extend T::Helpers
      
      module TestType
        extend T::Sig
        extend T::Helpers
        sealed!
        
        class OneConstructor < T::Struct; include RemoveConstructor::TestType; end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          ctor_name, ctor_json = json.first
          case ctor_name
            when "OneConstructor"
              OneConstructor.from_h(ctor_json)
          end
        end
      end
      
      class TestType::OneConstructor
        extend T::Sig
        extend T::Helpers
        
        prop :optional_field, T.nilable(Integer)
        prop :list_field, T::Array[Integer]
        prop :field, Integer
        
        sig { returns(Hash) }
        def to_h
          Hash["OneConstructor", {
            "optionalField": optional_field && optional_field,
            "listField": list_field.map { |elem| elem },
            "field": field,
          }]
        end
        
        sig { params(json: Hash).returns(T.self_type) }
        def self.from_h(json)
          new(
            optional_field: json["optionalField"] && json["optionalField"],
            list_field: (json["listField"] || []).map { |elem| elem },
            field: json["field"],
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