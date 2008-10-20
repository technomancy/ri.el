require 'rubygems'
gem 'miniunit'
require 'test/unit'
load File.dirname(__FILE__) + '/ri_repl'

class TestRiRepl < Test::Unit::TestCase
  def test_complete_absolute_method
    # instance
    completions = RDoc::RI.complete_absolute_method "String#sc"
    assert_at_least ["String#scan", "String#scanf"], completions

    completions = RDoc::RI.complete_absolute_method "String#"
    assert_at_least ["String#scan", "String#scanf"], completions

    # class
    completions = RDoc::RI.complete_absolute_method "YAML::add"
    assert_at_least ['YAML::add_builtin_type',
                     'YAML::add_domain_type',
                     'YAML::add_ruby_type'], completions

    completions = RDoc::RI.complete_absolute_method "YAML::"
    assert_at_least ['YAML::add_builtin_type',
                     'YAML::add_domain_type',
                     'YAML::add_ruby_type'], completions
    
    # dotted
    completions = RDoc::RI.complete_absolute_method "YAML.loa"
    assert_at_least ['YAML.load',
                     'YAML.load_file',
                     'YAML.load_documents'], completions

    # coloned
    completions = RDoc::RI.complete_absolute_method "YAML::loa"
    assert_at_least ['YAML::load',
                     'YAML::load_file',
                     'YAML::load_documents'], completions
  end

  def test_complete_constant
    completions = RDoc::RI.complete_constant "WEBrick::Cookie"
    assert_at_least ['WEBrick::Cookie'], completions
  end

  def test_complete_method_any_class
    completions = RDoc::RI.complete_method_any_class "execu"
    assert_at_least ['File::executable?',
                     'Pathname#executable?',
                     'IRB::ExtendCommand::Jobs#execute'], completions
  end

  def test_general_complete
    # Method with no name given
    completions = RDoc::RI.complete "String#"
    assert_at_least ['String#scan'], completions.split("\n")

    # Method or constant
    completions = RDoc::RI.complete "RDoc::"
    assert_at_least ['RDoc::RI', 'RDoc::usage'], completions.split("\n")
  end

  private
  def assert_at_least(expected, actual)
    assert((expected - actual).empty?,
           "#{actual.inspect} is missing #{(expected - actual).inspect}.")
  end
end
