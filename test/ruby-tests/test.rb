require 'minitest/autorun'
load 'api.rb'

class TestApi < MiniTest::Unit::TestCase
  def test_api
    assert_equal "Hi!", "Hi!"
  end
end
