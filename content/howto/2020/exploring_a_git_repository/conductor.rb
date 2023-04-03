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

  [ :monthly_commits, :monthly_commits_show_blanks, :project_timeline, :worktime].each do |transform|
    puts "Running #{transform}.rb"
    system "ruby #{transform}.rb #{commits_log} #{workdir}/#{transform}.csv"
  end
  system "cp *html #{workdir}"
end

def sync_and_analyze repo
  repo_dir = sync repo
  analyze repo_dir
end

[ "https://github.com/ruby-git/ruby-git",
  "https://github.com/camping/camping",
  "https://github.com/huginn/huginn",
  "https://github.com/alpinejs/alpine.git",
  "https://github.com/hundredrabbits/Dotgrid.git",
  "https://github.com/plotly/plotly.js.git",
  "https://github.com/mouse-reeve/fedireads.git",
  "https://github.com/alpinejs/alpine.git",
  "https://github.com/trailofbits/algo.git",
  "https://github.com/lovasoa/dezoomify.git",
  "https://github.com/szymonkaliski/archivist.git",
  "https://github.com/pikapkg/snowpack-init.git",
  "https://github.com/dominictarr/scalable-secure-scuttlebutt.git",
  "https://github.com/fpereiro/backendlore.git",
  "https://github.com/yusefnapora/pixelbook-linux.git",
  "https://github.com/sissbruecker/linkding.git",
  "https://github.com/hakimel/reveal.js.git",
  "https://github.com/automaticmode/active_workflow.git",
  "https://github.com/z0al/unread.git",
  "https://github.com/ipfs-shipyard/react-ipfs-url.git",
  "https://github.com/3b1b/manim.git",
  "https://github.com/mathieudutour/medium-to-own-blog.git",
  "https://github.com/marylychee/ssb-website-2.0.git",
  "https://github.com/ianstormtaylor/slate.git",
  "https://github.com/dangerousbeans/secret-islands.git",
  "https://github.com/davedx/mars-power.git",
  "https://github.com/dangerousbeans/vue-ssb.git",
  "https://github.com/cimryan/teslausb.git",
  "https://github.com/GoogleChromeLabs/comlink.git",
  "https://github.com/minamarkham/formation.git",
  "https://github.com/ipfs-shipyard/ipfs-deploy.git",
  "https://github.com/0x20/hackerspace-blueprint.git",
  "https://github.com/vadimdemedes/pastel.git",
  "https://github.com/cjdelisle/cjdns.git",
  "http://git.sv.gnu.org/r/guile.git"].each do |remote|
  sync_and_analyze remote
end
