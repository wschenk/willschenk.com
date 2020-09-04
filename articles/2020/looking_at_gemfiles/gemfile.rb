#!/usr/bin/env ruby
require 'bundler'

context = Bundler.load

puts "Dependencies"
context.dependencies.each do |x|
  printf "%-20s %s\n", x.name, x.requirements_list.to_s
end

puts
context.specs.each do |s|
  code = s.metadata['source_code_url'] || s.metadata['source_code_uri']
  puts "Gem from #{s.source.to_s}"
  printf "%-20s %-10s %-40s %s\n", s.name, s.version.to_s, code, s.homepage
end
