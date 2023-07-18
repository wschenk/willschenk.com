require 'erb'
require 'tilt'
require 'tilt/erb'

outer = Tilt::ERBTemplate.new('layout.erb')
html = outer.render( binding, title: nil ) do
  Tilt::ERBTemplate.new( "simple.erb" ).render
end

puts html
