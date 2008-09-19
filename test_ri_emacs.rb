require 'test/unit'
require File.dirname(__FILE__) + '/ri-emacs.rb'

class TestRiEmacs < Test::Unit::TestCase
  include RDoc::RI

  def setup
    @in = StringIO.new
    @out = StringIO.new
    @ri = RDoc::RI::Emacs.new
  end

  # Completion

  def test_complete_classes
    assert_include(['Class', 'ClassInheritableAttributes'],
                   @ri.complete('Cl'))
  end

  def test_complete_nested_class
    assert_include(['RDoc::RDocError', 'RDoc::RI', 'RDoc::RubyToken'],
                   @ri.complete('RDoc::R'))
  end

  def test_complete_instance_methods
    assert_include(['Array#at', 'Array#abbrev', 'Array#assoc'],
                   @ri.complete('Array#a'))
  end

  def test_complete_class_methods
    assert_include(['Array::new', 'Array#nitems'], @ri.complete('Array.n'))
  end

  #   def test_complete_methods_multiple_places
  #     assert_include(['Array#shift', 'Array#unshift', 'Hash#shift', 'Queue#shift'],
  #                    @ri.complete('shift'))
  #   end

  private
  def assert_include(included, includer)
    included.each { |i| assert(includer.include?(i),
                               "#{includer.inspect} did not include #{i.inspect}") }
  end
end
