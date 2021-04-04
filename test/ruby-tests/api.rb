require "json"
require "net/http"
require "uri"
require "sorbet-runtime"

class Api
  
  extend T::Sig
  extend T::Helpers
  
  class Person < T::Struct; end
  
  class Hobby < T::Struct; end
  
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
        hobbies: json["hobbies"].map { |elem| Hobby.from_h(elem) },
        first_name: json["firstName"],
      )
    end
  end
  
  class Hobby
    extend T::Sig
    extend T::Helpers
    
    prop :description, String
    
    sig { returns(Hash) }
    def to_h
      {
        "description": description,
      }
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
    
    body = ["get_person_by_id", arg]
    res = @http.request(req, body.to_json)
    json = JSON.parse(res.body)
    Person.from_h(json) unless json.empty?
  end
  
  sig { params(arg: NilClass).returns(T::Array[Person]) }
  def get_all_people(arg)
    req = Net::HTTP::Post.new(@origin)
    req["Content-Type"] = "application/json"
    
    body = ["get_all_people", arg]
    res = @http.request(req, body.to_json)
    json = JSON.parse(res.body)
    json.map { |elem| Person.from_h(elem) }
  end
end