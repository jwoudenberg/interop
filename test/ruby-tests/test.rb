require 'minitest/autorun'
load 'apis/example_api.rb'
load 'apis/v2/add_constructor.rb'

class TestApi < MiniTest::Unit::TestCase
  def test_api
    api = Apis::ExampleApi.new("http://localhost:#{ENV['PORT'].to_i}")
    response = api.get_person_by_id(42)
    expected =
      Apis::ExampleApi::Person.new(
        first_name: "Jasper",
        last_name: "Woudenberg",
        hobbies: [Apis::ExampleApi::Hobby::BoardGames.new],
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_add_constructor
    api = Apis::V2::AddConstructor.new("http://localhost:#{ENV['PORT'].to_i}")
    request = Apis::V2::AddConstructor::TestType::OtherConstructor.new
    response = api.echo(request)
    assert_equal response.to_h, request.to_h
  end
end
