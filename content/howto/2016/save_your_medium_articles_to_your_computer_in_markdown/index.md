---
title: Save your Medium articles to your computer in markdown
subtitle: sync everything
date: 2016-10-27T22:34:00.016Z
tags:
  - howto
  - medium
obsolete: true
aliases:
  - "/articles/2016/save_your_medium_articles_to_your_computer_in_markdown/"
---

I really like the editing tools and community on Medium, but I want to keep all my writing in a centralized place. So I wrote a script based upon my [dataflow programming with Rake post](http://willschenk.com/using-rake-for-dataflow-programming-and-data-science/) that pulls down my latest medium articles, converts to markdown, and downloads any associated images so I can keep my own copy on my domain.

I really like the tools, but I want to own my data.

1. Save the file below as “Rakefile”
2. install the upmark gem by typing “upmark”
3. replace “wschenk” with your username on line 5
4. type “rake”

```ruby
require "upmark"
require 'csv'
require 'fileutils'

user = "wschenk"
workingdir = "medium"
outputdir = "articles"

def url( dest, source )
  file dest do
    puts "Loading #{source}"
    if !File.exists?( dest )
      mkdir_p dest.to_s.pathmap( "%d" )
      sh "curl -L '#{source}' > #{dest}"
    end
  end
end

def file_loop( name, source )
  task name => source do
    if source =~ /.csv$/
      CSV.open( source ).each do |line|
        yield line
      end
    else
      File.readlines( source ).each do |line|
        yield line
      end
    end
  end
end

# Parse an HTML file into CSV
def parse_html( dest, source, &parser )
  require 'nokogiri'
  require 'csv'

  file dest => source do
    puts "Parsing #{source} -> #{dest}"
    mkdir_p dest.to_s.pathmap( "%d" )

    html = Nokogiri.parse( File.read( source ) )
    CSV.open( dest.to_s, "wb" ) do |csv|
      parser.call( html, csv )
    end
  end
end

url "#{workingdir}/latest.html", "https://medium.com/@#{user}/latest"

parse_html "#{workingdir}/latest_urls.csv", "#{workingdir}/latest.html" do |html,out|
  latest = html.css( "h4" ).first
  latest.parent.parent.children.each do |div|
    title = div.css("h1")
    link = div.css("a")
    if link.last
      link = link.last.attr( "href" )
      link = link.gsub( /\?.*/, "")
      link = "https://medium.com#{link}"
      puts title.text
      puts link
      out << [title.text,link]
    end
  end
end
#   html.css( ".streamItem--postPreview").each do |div|
#     title = div.css( "h3").text
#     link = div.css( 'a[data-action="open-post"]' )
#     link = link.first['href']
#     link = link.gsub( /\?.*/, "")
#     out << [title,link]
#   end
# end

file_loop "download_and_convert_to_md", "#{workingdir}/latest_urls.csv" do |line|
  title = clean_title line[0]
  file = "#{workingdir}/articles/#{title}"
  url( "#{file}.article.html", line[1] )
  Rake::Task["#{file}.article.html"].invoke
  Rake::Task["#{file}.md"].invoke
end

def clean_title full
  full.downcase.gsub( /’/, "" ).gsub(/[^0-9a-z]/, "_" ).gsub( /__/, "_" )
end

rule ".md" => ".article.html" do |dest|
  html = Nokogiri.parse( File.read( dest.source ) )
  article = html.css( ".postArticle-content" ).first

  image_name = File.basename dest.to_s, ".md"
  image_count = 0

  File.open( dest.to_s, "w" ) do |out|
    out.puts "---"
    out.puts "title: #{article.css("h1").first.text}"

    time = html.css( "time" ).first.attr( "datetime" )
    out.puts "date: #{time}"
    out.puts "---"
    article.css( "h1,h2,h3,h4,p,blockquote,ol,ul,hr,img" ).each do |elem|
    # puts elem.to_s
    case elem.name.downcase
      when 'hr'
        out.puts "* * *"
      when 'blockquote'
        out.puts "> #{Upmark.convert( elem.text )}"
      when 'img'
        src_url = elem.attributes['src']
        type = File.extname src_url
        image_count += 1
        output_name = "#{workingdir}/articles/#{image_name}_#{image_count}#{type}"
        url( output_name, src_url )
        Rake::Task[output_name].invoke
        out.puts "<img src='#{image_name}_#{image_count}#{type}'/>"
      else
        out.puts Upmark.convert( elem.to_s )
      end
      out.puts
    end
  end
end

task :articles => [:download_and_convert_to_md, :copy_to_final]
file_loop :copy_to_final, "#{workingdir}/latest_urls.csv" do |line|
  title = clean_title line[0]
  outdir = "#{outputdir}/#{title}"
  mkdir_p outdir
  cp "#{workingdir}/articles/#{title}.md", "#{outdir}/index.md"
  Dir.glob( "#{workingdir}/articles/#{title}_*").each do |r|
    cp r, outdir
  end
end

task :reload do
  Dir.glob( "#{workingdir}/articles/*md" ).each do |f|
    File.unlink f
  end
end

task default: :articles
```
