require 'minitest/autorun'
load 'apis/example_api.rb'
load 'apis/v2/add_constructor.rb'
load 'apis/v2/add_list_field.rb'
load 'apis/v2/add_optional_field.rb'
load 'apis/v2/drop_list_field.rb'
load 'apis/v2/drop_optional_field.rb'
load 'apis/v2/remove_constructor.rb'

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

  def test_add_list_field
    api = Apis::V2::AddListField.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::V2::AddListField::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
        other_list_field: [4,5,6],
      )
    response = api.echo(request)
    expected =
      Apis::V2::AddListField::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
        other_list_field: [],
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_add_optional_field
    api = Apis::V2::AddOptionalField.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::V2::AddOptionalField::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
        new_optional_field: 9,
      )
    response = api.echo(request)
    expected =
      Apis::V2::AddOptionalField::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
        new_optional_field: nil,
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_drop_list_field
    api = Apis::V2::DropListField.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::V2::DropListField::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
      )
    response = api.echo(request)
    expected =
      Apis::V2::DropListField::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_drop_optional_field
    api = Apis::V2::DropOptionalField.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::V2::DropOptionalField::TestType::OneConstructor.new(
        field: 1,
        list_field: [1,2,3],
      )
    response = api.echo(request)
    expected =
      Apis::V2::DropOptionalField::TestType::OneConstructor.new(
        field: 1,
        list_field: [1,2,3],
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_remove_constructor
    api = Apis::V2::RemoveConstructor.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::V2::RemoveConstructor::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
      )
    response = api.echo(request)
    expected =
      Apis::V2::RemoveConstructor::TestType::OneConstructor.new(
        field: 1,
        optional_field: 2,
        list_field: [1,2,3],
      )
    assert_equal response.to_h, expected.to_h
  end
end
