require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'front_matter_parser', "1.0.1"
  gem 'erb'
end

puts "FrontMatterParser #{FrontMatterParser::VERSION}"

if !Dir.exist? "src"
  puts "Creating src"
  FileUtils.mkdir_p "src"
end

def process( file )
  parsed = FrontMatterParser::Parser.parse_file(file)

  p parsed.front_matter
  p parsed.content

  title = parsed.front_matter["title"] ||= "Default Title"

  template = ERB.new parsed.content

  inner = template.run(binding)

  template = ERB.new File.read( "src/_layout.html" )

  main = template.run(binding)
  puts main
end


process "src/index.html"
