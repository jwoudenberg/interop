require 'minitest/autorun'
load 'apis/example_api.rb'
load '../ExampleApiChanges/AddConstructor/v2.rb'
load '../ExampleApiChanges/AddFirstField/v2.rb'
load '../ExampleApiChanges/AddListField/v2.rb'
load '../ExampleApiChanges/AddOptionalField/v2.rb'
load '../ExampleApiChanges/DropListField/v2.rb'
load '../ExampleApiChanges/DropOptionalField/v2.rb'
load '../ExampleApiChanges/RemoveConstructor/v2.rb'

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

  def test_dictionary_response
    api = Apis::ExampleApi.new("http://localhost:#{ENV['PORT'].to_i}")
    response = api.get_all_people(nil)
    expected =
      Apis::ExampleApi::Person.new(
        first_name: "Jasper",
        last_name: "Woudenberg",
        hobbies: [Apis::ExampleApi::Hobby::BoardGames.new],
      )
    assert_equal response[42].to_h, expected.to_h
  end

  def test_add_constructor
    api = Apis::AddConstructor::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request = Apis::AddConstructor::V2::TestType::OtherConstructor.new
    response = api.echo(request)
    assert_equal response.to_h, request.to_h
  end

  def test_add_list_field
    api = Apis::AddListField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::AddListField::V2::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
        other_list_field: [4,5,6],
      )
    response = api.echo(request)
    expected =
      Apis::AddListField::V2::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
        other_list_field: [],
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_add_optional_field
    api = Apis::AddOptionalField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::AddOptionalField::V2::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
        new_optional_field: 9,
      )
    response = api.echo(request)
    expected =
      Apis::AddOptionalField::V2::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
        new_optional_field: nil,
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_add_first_field
    api = Apis::AddFirstField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::AddFirstField::V2::TestType::OtherConstructor.new(
        new_field: 1,
      )
    response = api.echo(request)
    expected =
      Apis::AddFirstField::V2::TestType::OtherConstructor.new(
        new_field: nil,
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_drop_list_field
    api = Apis::DropListField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::DropListField::V2::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
      )
    response = api.echo(request)
    expected =
      Apis::DropListField::V2::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_drop_optional_field
    api = Apis::DropOptionalField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::DropOptionalField::V2::TestType::OneConstructor.new(
        field: 1,
        list_field: [1,2,3],
      )
    response = api.echo(request)
    expected =
      Apis::DropOptionalField::V2::TestType::OneConstructor.new(
        field: 1,
        list_field: [1,2,3],
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_remove_constructor
    api = Apis::RemoveConstructor::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::RemoveConstructor::V2::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
      )
    response = api.echo(request)
    expected =
      Apis::RemoveConstructor::V2::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
      )
    assert_equal response.to_h, expected.to_h
  end
end
