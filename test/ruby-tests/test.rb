require 'minitest/autorun'
load 'api.rb'

class TestApi < MiniTest::Unit::TestCase
  def test_api
    api = Api.new("http://localhost:#{ENV['PORT'].to_i}")
    response = api.get_person_by_id(42)
    expected =
      Api::Person.new(
        first_name: "Jasper",
        last_name: "Woudenberg",
        hobbies: [Api::Hobby::BoardGames.new],
      )
    assert_equal response.to_h, expected.to_h
  end
end
