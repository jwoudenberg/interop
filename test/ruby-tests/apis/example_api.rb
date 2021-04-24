require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

module Apis
  class ExampleApi
    
    extend T::Sig
    extend T::Helpers
    
    class Person < T::Struct; end
    
    module Hobby
      extend T::Sig
      extend T::Helpers
      sealed!
      
      class Piano < T::Struct; include ExampleApi::Hobby; end
      class Football < T::Struct; include ExampleApi::Hobby; end
      class BoardGames < T::Struct; include ExampleApi::Hobby; end
      
      sig { params(json: Hash).returns(T.self_type) }
      def self.from_h(json)
        ctor_name, ctor_json = json.first
        case ctor_name
          when "Piano"
            Piano.from_h(ctor_json)
          when "Football"
            Football.from_h(ctor_json)
          when "BoardGames"
            BoardGames.from_h(ctor_json)
        end
      end
    end
    
    class Person
      extend T::Sig
      extend T::Helpers
      
      prop :last_name, String
      prop :hobbies, T::Array[Hobby]
      prop :first_name, String
      
      sig { returns(Hash) }
      def to_h
        {
          "lastName": last_name,
          "hobbies": hobbies.map { |elem| elem.to_h },
          "firstName": first_name,
        }
      end
      
      sig { params(json: Hash).returns(T.self_type) }
      def self.from_h(json)
        new(
          last_name: json["lastName"],
          hobbies: (json["hobbies"] || []).map { |elem| Hobby.from_h(elem) },
          first_name: json["firstName"],
        )
      end
    end
    
    class Hobby::Piano
      extend T::Sig
      extend T::Helpers
      
      
      
      sig { returns(Hash) }
      def to_h
        Hash["Piano", {
          
        }]
      end
      
      sig { params(json: Hash).returns(T.self_type) }
      def self.from_h(json)
        new(
          
        )
      end
    end
    
    class Hobby::Football
      extend T::Sig
      extend T::Helpers
      
      
      
      sig { returns(Hash) }
      def to_h
        Hash["Football", {
          
        }]
      end
      
      sig { params(json: Hash).returns(T.self_type) }
      def self.from_h(json)
        new(
          
        )
      end
    end
    
    class Hobby::BoardGames
      extend T::Sig
      extend T::Helpers
      
      
      
      sig { returns(Hash) }
      def to_h
        Hash["BoardGames", {
          
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
    
    sig { params(arg: Integer).returns(T.nilable(Person)) }
    def get_person_by_id(arg)
      req = Net::HTTP::Post.new(@origin)
      req["Content-Type"] = "application/json"
      
      body = ["get_person_by_id", arg]
      res = @http.request(req, body.to_json)
      json = JSON.parse(res.body)
      json && Person.from_h(json)
    end
    
    sig { params(arg: NilClass).returns(T::Hash[Integer, Person]) }
    def get_all_people(arg)
      req = Net::HTTP::Post.new(@origin)
      req["Content-Type"] = "application/json"
      
      body = ["get_all_people", []]
      res = @http.request(req, body.to_json)
      json = JSON.parse(res.body)
      (json || []).map { |key, val| [key, Person.from_h(val)] }.to_h
    end
  end
end
# INTEROP-SPEC:{"endpoints":{"get_all_people":{"requestType":{"tag":"Unit"},"responseType":{"tag":"Dict","contents":[{"tag":"Int"},{"tag":"NestedCustomType","contents":"Person"}]}},"get_person_by_id":{"requestType":{"tag":"Int"},"responseType":{"tag":"Optional","contents":{"tag":"NestedCustomType","contents":"Person"}}}},"customTypes":{"Person":{"subTypes":{"Left":[{"fieldType":{"tag":"Text"},"fieldName":"firstName"},{"fieldType":{"tag":"Text"},"fieldName":"lastName"},{"fieldType":{"tag":"List","contents":{"tag":"NestedCustomType","contents":"Hobby"}},"fieldName":"hobbies"}]},"typeName":"Person"},"Hobby":{"subTypes":{"Right":[{"constructorName":"BoardGames","fields":[]},{"constructorName":"Piano","fields":[]},{"constructorName":"Football","fields":[]}]},"typeName":"Hobby"}}}