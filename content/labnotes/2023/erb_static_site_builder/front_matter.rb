require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'front_matter_parser', "1.0.1"
  gem 'tilt'
  gem 'kramdown'
end

require 'tilt/erb'
require 'tilt/kramdown'

def process( file )
  puts "\nProcessing #{file}"
  parsed = FrontMatterParser::Parser.parse_file(file)

  title = parsed.front_matter["title"] ||= "Default Title"

  outer = Tilt::ERBTemplate.new('layout.erb', default_encoding: 'UTF-8' )
  res = outer.render( binding, title: title ) do
    results = ERB.new( parsed.content ).result( binding )

    if file =~ /.md$/
      Kramdown::Document.new(results).to_html
    else
      results
    end
  end

  puts res
end

process "front_matter.html"
process "front_matter.md"
