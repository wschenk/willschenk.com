#!/usr/bin/env ruby
require 'bundler'

def describe_lockfile file = Bundler.default_lockfile
  context = Bundler::LockfileParser.new( Bundler.read_file( file ) )
  puts "Bundler version"
  puts context.bundler_version.to_s

  puts
  puts "Dependencies"
  context.dependencies.each do |name,x|
    printf "%-20s %s\n", x.name, x.requirements_list.to_s
  end

  puts
  puts "Gems"
  context.specs.each do |s|
    printf "%-20s %-10s\n", s.name, s.version.to_s
    printf "%-10s %s\n", s.source.class, s.source.to_s
  end
end

describe_lockfile
