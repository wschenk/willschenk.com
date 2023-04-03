require 'json'

WORKDIR="/tmp/repos"

Fingerprint = Struct.new :dir,
                         :license,
                         :readme,
                         :gemfile,
                         :gemfilelock,
                         :gemspec,
                         :packagejson,
                         :packagelockjson,
                         :dockerfile,
                         :changelog,
                         :funding,
                         :maintainers,
                         :issue_template,
                         :pull_request_template,
                         :ci

def patterncheck dir, pattern
  Dir.entries( dir ).select do |f|
    f =~ /#{pattern}/i
  end
end

def exactcheck dir, pattern
  if Dir.exist? dir
    Dir.entries( dir ).select do |f|
      f == pattern
    end
  end
end

def repo_checker dir
  i = Fingerprint.new

  i.dir = dir
  i.license = patterncheck dir, "license"
  i.readme = patterncheck dir, "readme"
  i.gemfile = exactcheck dir, "Gemfile"
  i.gemfilelock = exactcheck dir, "Gemfile.lock"
  i.gemspec = patterncheck dir, ".gemspec"
  i.packagejson = exactcheck dir, "package.json"
  i.packagelockjson = exactcheck dir, "package-lock.json"
  i.dockerfile = exactcheck dir, "Dockerfile"
  i.changelog = patterncheck dir, "changelog"
  i.maintainers = patterncheck dir, "maintainers"
  i.issue_template = patterncheck dir, "issue_template"
  i.funding = exactcheck "#{dir}/.github", "FUNDING.yml"
  i.pull_request_template = patterncheck dir, "pull_request_template"

  i
end

`find #{WORKDIR} -name repo -type d -print`.each_line do |dir|
  dir.chomp!
  info = repo_checker dir

  if info.changelog
    puts "#{dir} changelog"
  end
  File.open( "#{dir}/../profile.json", "w" ) { |out| out.puts info.to_h.to_json }


end
