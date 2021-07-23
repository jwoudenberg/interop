require 'minitest/autorun'
load '../ExampleApis/EchoTypes/api.rb'
load '../ExampleApis/AddConstructor/v2.rb'
load '../ExampleApis/AddFirstField/v2.rb'
load '../ExampleApis/AddFirstFieldToSecondConstructor/v2.rb'
load '../ExampleApis/AddListField/v2.rb'
load '../ExampleApis/AddOptionalField/v2.rb'
load '../ExampleApis/DropListField/v2.rb'
load '../ExampleApis/DropOptionalField/v2.rb'
load '../ExampleApis/RemoveConstructor/v2.rb'
load '../ExampleApis/AddDictField/v2.rb'

class TestApi < MiniTest::Unit::TestCase
  def test_api
    api = Apis::EchoTypes::Api.new("http://localhost:#{ENV['PORT'].to_i}")
    response = api.get_person_by_id(42)
    expected =
      Apis::EchoTypes::Api::Person.new(
        first_name: "Jasper",
        last_name: "Woudenberg",
        hobbies: [Apis::EchoTypes::Api::Hobby::BoardGames.new],
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_dictionary_response
    api = Apis::EchoTypes::Api.new("http://localhost:#{ENV['PORT'].to_i}")
    response = api.get_all_people(nil)
    expected =
      Apis::EchoTypes::Api::Person.new(
        first_name: "Jasper",
        last_name: "Woudenberg",
        hobbies: [Apis::EchoTypes::Api::Hobby::BoardGames.new],
      )
    assert_equal response[42].to_h, expected.to_h
  end

  def test_add_constructor
    api = Apis::AddConstructor::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request = Apis::AddConstructor::V2::AddConstructorType::SecondConstructor.new
    response = api.add_constructor(request)
    assert_equal response.to_h, request.to_h
  end

  def test_add_list_field
    api = Apis::AddListField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::AddListField::V2::AddListFieldType.new(
        field: 1,
        other_list_field: [4,5,6],
      )
    response = api.add_list_field(request)
    expected =
      Apis::AddListField::V2::AddListFieldType.new(
        field: 1,
        other_list_field: [],
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_add_dict_field
    api = Apis::AddDictField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::AddDictField::V2::AddDictFieldType.new(
        field: 1,
        other_dict_field: { 1 => 0.5, 5 => 0.33 },
      )
    response = api.add_dict_field(request)
    expected =
      Apis::AddDictField::V2::AddDictFieldType.new(
        field: 1,
        other_dict_field: {},
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_add_optional_field
    api = Apis::AddOptionalField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::AddOptionalField::V2::AddOptionalFieldType.new(
        field: 1,
      )
    response = api.add_optional_field(request)
    expected =
      Apis::AddOptionalField::V2::AddOptionalFieldType.new(
        field: 1,
        new_optional_field: nil,
      )
    assert_equal response.to_h, expected.to_h
  end

  def add_first_field
    api = Apis::AddFirstField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::AddFirstField::V2::AddFirstFieldType.new(
        new_field: 1,
      )
    response = api.add_first_field_to_second_constructor(request)
    expected =
      Apis::AddFirstField::V2::AddFirstFieldType.new(
        new_field: nil,
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_add_first_field_to_second_constructor
    api = Apis::AddFirstFieldToSecondConstructor::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::AddFirstFieldToSecondConstructor::V2::AddFirstFieldToSecondConstructorType::AddFirstFieldSecondConstructor.new(
        new_field: 1,
      )
    response = api.add_first_field_to_second_constructor(request)
    expected =
      Apis::AddFirstFieldToSecondConstructor::V2::AddFirstFieldToSecondConstructorType::AddFirstFieldSecondConstructor.new(
        new_field: nil,
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_drop_list_field
    api = Apis::DropListField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::DropListField::V2::DropListFieldType.new(
        field: 1,
      )
    response = api.drop_list_field(request)
    expected =
      Apis::DropListField::V2::DropListFieldType.new(
        field: 1,
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_drop_optional_field
    api = Apis::DropOptionalField::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::DropOptionalField::V2::DropOptionalFieldType.new(
        field: 1,
      )
    response = api.drop_optional_field(request)
    expected =
      Apis::DropOptionalField::V2::DropOptionalFieldType.new(
        field: 1,
      )
    assert_equal response.to_h, expected.to_h
  end

  def test_remove_constructor
    api = Apis::RemoveConstructor::V2.new("http://localhost:#{ENV['PORT'].to_i}")
    request =
      Apis::RemoveConstructor::V2::RemoveConstructorType::KeepThisConstructor.new()
    response = api.remove_constructor(request)
    expected =
      Apis::RemoveConstructor::V2::RemoveConstructorType::KeepThisConstructor.new()
    assert_equal response.to_h, expected.to_h
  end
end
