#!/usr/bin/env ruby

require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'

  gem 'httparty', '~> 0.21.0'
  gem 'nokogiri',  '~> 1.14'
end

require 'uri'

def internal_links(base_url)
  response = HTTParty.get(base_url)
  parsed_page = Nokogiri::HTML(response.body)

  links = parsed_page.css('a').map do |link|
    href = link.attributes['href'].to_s
    url = URI.join(base_url, href).to_s
    url if url.start_with?(base_url)
  end.compact.uniq

  links
end

def save_links_to_file(links, file_name)
  File.open(file_name, 'w') do |file|
    links.each { |link| file.puts(link) }
  end
end

def read_links_from_file(file_name)
  File.readlines(file_name).map(&:strip)
end

def check_links(links, local_base_url)
  File.open( "bad_links.txt", "wb" ) do |out|
  links.each do |link|
    local_url = link.sub('https://willschenk.com', local_base_url)
    response = HTTParty.get(local_url)

    if response.code == 200
      # puts "#{local_url} - OK"
    else
      out << local_url
      out << "\n"
      puts "#{local_url} - ERROR (#{response.code})"
    end
  end
end
end

base_url = 'https://willschenk.com'
local_base_url = 'http://localhost:1313'
links_file = 'links.txt'

if !File.exists?( links_file )
  # First pass: Crawl the original website and store the links in a file
  puts "Crawling the original website..."
  links = internal_links(base_url)
  save_links_to_file(links, links_file)
  puts "Links saved to #{links_file}."
end

# Second pass: Read the links from the file and check them against the new website
puts "Reading links from #{links_file} and checking the new website..."
links = read_links_from_file(links_file)
check_links(links, local_base_url)
puts "Link checking finished."
