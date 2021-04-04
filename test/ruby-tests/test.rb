require 'sorbet-runtime'

module Foo
  extend T::Sig

  sig { params(greeting: String).void }
  def self.print_msg(greeting)
    puts greeting
  end
end

Foo.print_msg('Hello, World!')
