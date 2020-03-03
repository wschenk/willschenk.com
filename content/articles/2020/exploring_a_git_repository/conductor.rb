#!/usr/bin/env ruby
require 'fileutils'

WORKDIR=ENV['CONDUCTOR_WORKDIR'] || "#{ENV['HOME']}/gratitude"
FileUtils.mkdir_p WORKDIR

def sync( remote, name = nil )
  # Right now assume github
  throw "Need to update path for non github urls" unless remote =~ /github.com/

  name = remote.gsub( /.*\//, "" ).gsub( /\.git$/, "" )

  repo_dir = "#{WORKDIR}/#{name}/repository"

  puts "Looking for repo in #{repo_dir}"
  
  if Dir.exists? repo_dir
    system( "GIT_DIR=#{repo_dir} git fetch origin master" )
  else
    system( "git clone --bare #{remote} #{repo_dir}" )
  end

  repo_dir
end

def analyze( repo_dir )
  throw "#{repo_dir} should be a bare repository named 'repository'" unless repo_dir =~ /\/repository/

  workdir = repo_dir.gsub( /\/repository/, "" )

  commits_log = "#{workdir}/commits.log"
  
  system "GIT_DIR=#{repo_dir} git log --reverse --pretty='format:%aI|%ae|%an|%D' | sort > #{commits_log}"

  system "ruby project_timeline.rb #{commits_log} #{workdir}/project_timeline.csv"
end

def sync_and_analyze repo
  repo_dir = sync repo
  analyze repo_dir
end

sync_and_analyze "https://github.com/ruby-git/ruby-git"
sync_and_analyze "https://github.com/camping/camping"
sync_and_analyze "https://github.com/huginn/huginn"
sync_and_analyze "https://github.com/alpinejs/alpine.git"

