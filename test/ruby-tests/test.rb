require 'minitest/autorun'
load 'api.rb'

class TestApi < MiniTest::Unit::TestCase
  def test_api
    api = Api.new("http://localhost:9000")
    response = api.get_person_by_id(42)
    expected =
      Person::Person.new(
        first_name: "Jasper",
        las_name: "Woudenberg",
        hobbies: [
          Hobby::Hoby.new(
            description: "boardgames",
          ),
        ],
      )
    assert_equal response, expected
  end
end
