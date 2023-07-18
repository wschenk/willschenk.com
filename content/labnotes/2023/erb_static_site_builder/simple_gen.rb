#!/usr/bin/env ruby

require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'front_matter_parser', "1.0.1"
  gem 'tilt'
  gem 'kramdown'
end

require 'tilt/erb'
require 'tilt/kramdown'

DEFAULT_LAYOUT = <<-HTML
<html>
  <head>
    <title><%= title %></title>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/water.css@2/out/water.css"></link>
  </head>
<body>
 <nav>
   <ul>
     <% site.pages.each do |page| %>
       <li><a href="<%= page.link %>"><%= page.title %></a></li>
     <% end %>
   </ul>
 </nav>
 <%= yield %>
 </body>
</html>
HTML

class InputFile
  attr_accessor :file, :parsed, :front_matter, :content

  def initialize( file, prefix )
    @file = file[prefix.length+1..]
    @parsed = FrontMatterParser::Parser.parse_file(file)
    @front_matter = @parsed.front_matter
    @content = @parsed.content
  end

  def title
    return @file if front_matter.nil?
    front_matter["title"] ||= "Title"
  end

  def link
    if File.extname(file) == ".md"
      "/" + file.gsub( /.md/, ".html" )
    else
      "/" + file
    end
  end
end

class Site
  attr_accessor :dir

  def initialize( dir = 'input' )
    @dir = dir
    @files = {}
    Dir.glob( "#{dir}/**/*" ).each do |file|
      add_file file
    end
  end

  def add_file file
    if File.file? file
      f = InputFile.new( file, @dir )

      printf "%-15s %s\n", f.file, f.title
      if f.file == "_layout.html"
        @layout = f
      else
        @files[f.file] = f
      end
    else
      puts "Skipping #{file}"
    end
  end

  def layout
    Tilt::ERBTemplate.new do
      @layout.nil? ? DEFAULT_LAYOUT : @layout.content
    end
  end

  def pages
    @files.values
  end

  def generate
    FileUtils.mkdir_p "output"

    outer = layout

    @files.each do |key,file|
      res = outer.render( binding, title: file.title, site: self ) do
        results = ERB.new( file.content ).result( binding )

        if key =~ /.md$/
          Kramdown::Document.new(results).to_html
        else
          results
        end
      end

      output_file = "output#{file.link}"
      FileUtils.mkdir_p File.dirname( output_file )
      puts "Writing #{output_file}"

      File.open( output_file, "w" ) { |out| out << res }
    end
  end
end

s = Site.new
s.generate
