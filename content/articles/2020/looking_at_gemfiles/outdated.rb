#!/usr/bin/env ruby
require 'bundler'
require 'gems'

def specs_from_lockfile file = Bundler.default_lockfile
  gems = {}
  context = Bundler::LockfileParser.new(Bundler.read_file(file))
  context.specs.each do |info|
    gems[info.name] = {version: info.version.to_s, source: info.source.class }
    if info.source.is_a? Bundler::Source::Rubygems
      gems[info.name][:remote] = info.source.remotes.first.to_s 
    elsif info.source.is_a? Bundler::Source::Git
      gems[info.name][:remote] = info.source.uri 
      gems[info.name][:ref] = info.source.ref
      gems[info.name][:revision] = info.source.revision
    else
      puts "Not sure how to process #{info.source.class}"
    end
  end
  gems
end

def add_rubygems_versions info
  info.each do |name, spec|
    if spec[:source] == Bundler::Source::Rubygems
      gems_client = Gems::Client.new( { host: spec[:remote] } )
      spec[:info] = gems_client.info( name )
    elsif spec[:source] == Bundler::Source::Git
      spec[:info] = Gems.info( name )
    else
      puts "Not sure of the source of #{name}"
    end
  end
end

specs = specs_from_lockfile ARGV[0] || Bundler.default_lockfile
add_rubygems_versions( specs )

printf "%15s  %-8s %-8s %3s %s\n", "Name", "Current", "Latest", "Old", "Info"

specs.each do |name,info|
  info[:info] ||= {}
  current_version = info[:version]
  new_version = info[:info]["version"]
  printf "%15s  %-8s %-8s %-3s %s\n", 
         name,
         current_version,
         new_version,
         current_version == new_version ? "" : "Y",
         info[:info]["info"][0..50]
end
