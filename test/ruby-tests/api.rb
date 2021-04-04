require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

class Api
  
  extend T::Sig
  extend T::Helpers
  
  module Person
    extend T::Sig
    extend T::Helpers
    sealed!
    
    class Person < T::Struct; include Api::Person; end
    
    sig { params(json: Hash).returns(T.self_type) }
    def self.from_h(json)
      ctor_name, ctor_json = json.first
      case ctor_name
        when "Person"
          Person.from_h(ctor_json)
      end
    end
  end
  
  module Hobby
    extend T::Sig
    extend T::Helpers
    sealed!
    
    class Hobby < T::Struct; include Api::Hobby; end
    
    sig { params(json: Hash).returns(T.self_type) }
    def self.from_h(json)
      ctor_name, ctor_json = json.first
      case ctor_name
        when "Hobby"
          Hobby.from_h(ctor_json)
      end
    end
  end
  
  class Person::Person
    extend T::Sig
    extend T::Helpers
    
    prop :last_name, String
    prop :hobbies, T::Array[Hobby]
    prop :first_name, String
    
    sig { returns(Hash) }
    def to_h
      Hash["Person", {
        "lastName": last_name,
        "hobbies": hobbies.map { |elem| elem.to_h },
        "firstName": first_name,
      }]
    end
    
    sig { params(json: Hash).returns(T.self_type) }
    def self.from_h(json)
      new(
        last_name: json["lastName"],
        hobbies: json["hobbies"].map { |elem| Hobby.from_h(elem) },
        first_name: json["firstName"],
      )
    end
  end
  
  class Hobby::Hobby
    extend T::Sig
    extend T::Helpers
    
    prop :description, String
    
    sig { returns(Hash) }
    def to_h
      Hash["Hobby", {
        "description": description,
      }]
    end
    
    sig { params(json: Hash).returns(T.self_type) }
    def self.from_h(json)
      new(
        description: json["description"],
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
    
    res = @http.request(req, arg.to_json)
    json = JSON.parse(res.body)
    Person.from_h(json) unless json.empty?
  end
  
  sig { params(arg: NilClass).returns(T::Array[Person]) }
  def get_all_people(arg)
    req = Net::HTTP::Post.new(@origin)
    req["Content-Type"] = "application/json"
    
    res = @http.request(req, arg.to_json)
    json = JSON.parse(res.body)
    json.map { |elem| Person.from_h(elem) }
  end
end