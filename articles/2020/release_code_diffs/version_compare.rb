require 'pp'

WORKDIR='/tmp'

repo='https://github.com/middleman/middleman'
current_version='4.3.3'
new_version='4.3.9'

def load_repo repo
  dir = repo.gsub( /.*\//, "" )
  repo_dir = "#{WORKDIR}/#{dir}/repo"

  if File.exists? repo_dir
    system( "cd #{repo_dir};git pull origin master" )
  else
    system( "mkdir -p #{WORKDIR}/#{dir}" )
    system( "cd #{WORKDIR}/#{dir};git clone #{repo} repo" )
  end

  repo_dir
end

repo_dir = load_repo( repo )

puts "Repo directory: #{repo_dir}"

latest_tags = `cd #{repo_dir};git tag --sort=-v:refname`.lines.collect { |x| x.chomp }

puts "latest_tag : #{latest_tags.first}"

require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'semver2', '3.4.2', require: 'semver'
end

tag_info = latest_tags.collect { |tag| SemVer.parse_rubygems tag }

releases = tag_info.select { |x| x.prerelease == "" }
puts "latest_release : #{releases.first}"

def find_tag tag_info, latest_tags, version_string
  version = SemVer.parse_rubygems version_string

  tag_info.each_with_index do |tag, idx|
    if tag == version
      return latest_tags[idx]
    end
  end

  nil
end

current_tag = find_tag tag_info, latest_tags, current_version
new_tag = find_tag tag_info, latest_tags, new_version

if current_tag.nil?
  puts "Couldn't find current_version #{current_version}"
end

if new_tag.nil?
  puts "Couldn't find new_version #{new_version}"
end

if current_tag.nil? || new_tag.nil?
  exit 1
end

puts "current_tag: #{current_tag}"
puts "new_tag    : #{new_tag}"

def find_latest_patch tag_info, version_string
  version = SemVer.parse_rubygems version_string

  tag_info.each do |tag|
    return tag if tag.major == version.major && tag.minor = version.minor
  end

  nil
end

highest_patch = find_latest_patch tag_info, new_tag
puts "most compatible patch: #{highest_patch}"

git_cmd = "git log --reverse --pretty='format:%aI|%h|%ae|%an|%s' #{current_tag}..#{new_tag}"

commits = `cd #{repo_dir};#{git_cmd}`.lines.collect do |x|
  x.chomp
  md = /(.*?)\|(.*?)\|(.*?)\|(.*?)\|(.*)/.match( x )
  { date: md[1], hash: md[2], email: md[3], name: md[4], summary: md[5] }
end

puts "Most recent change: #{commits.first[:date]}"
puts "Oldest change     : #{commits.last[:date]}"

require 'time'
oldest = Time.parse( commits.first[:date] )
newest = Time.parse( commits.last[:date] )
days_passed = (newest - oldest) / (60 * 60 * 24) # Seconds in a day

puts "Days passed : #{days_passed}"

issues = commits.collect do |commit|
  md = /\#([^\s\)\]]*)/.match( commit[:summary] )
  md ? md[1] : nil
end.select { |x| x }.sort

pp issues

changes = {}

if File.exists? "#{repo_dir}/CHANGELOG.md"
  scan_version = nil
  entries = []
  headline_re = /\#{1,3} (.*)/

  File.read( "#{repo_dir}/CHANGELOG.md" ).each_line do |line|
    line.chomp!
    md = headline_re.match( line )
    if md
      if current_version && entries.length > 0
        changes[scan_version] = entries
        entries = []
      end
      scan_version = md[1]
    elsif line != ""
      entries << line
    end
  end

  if current_version && entries.length > 0
    changes[scan_version] = entries
  end
end

if changes[new_version]
  puts "Found change log for #{new_version}"
else
   puts "No entry for #{new_version}"
end

if changes[current_version]
  puts "Found change log for #{current_version}"
else
 puts "No entry for #{current_version}"
end

authors = {}
commits.each do |c|
  authors[c[:name]] ||= 0
  authors[c[:name]] += 1
end

authors.keys.sort { |a,b| authors[b] <=> authors[a] }.each do |author|
  printf "%10d %s\n", authors[author], author
end
