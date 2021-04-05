require 'minitest/autorun'
load 'apis/example_api.rb'

class TestApi < MiniTest::Unit::TestCase
  def test_api
    api = Apis::ExampleApi::Api.new("http://localhost:#{ENV['PORT'].to_i}")
    response = api.get_person_by_id(42)
    expected =
      Apis::ExampleApi::Api::Person.new(
        first_name: "Jasper",
        last_name: "Woudenberg",
        hobbies: [Apis::ExampleApi::Api::Hobby::BoardGames.new],
      )
    assert_equal response.to_h, expected.to_h
  end
end
