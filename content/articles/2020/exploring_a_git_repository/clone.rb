require 'git'

class GitExplorer
  attr_accessor :name, :remote, :workdir, :repo

  def initialize( name, remote, workdir = nil )
    @workdir = workdir || Dir.mktmpdir
    @remote = remote
    @name = name

    if !Dir.exist? "#{workdir}/#{name}"
      puts "Cloning #{remote} into #{workdir}"
      @repo = Git.clone remote, name, path: @workdir
    else
      @repo = Git.open "#{workdir}/#{name}/#{name}"
    end
  end

  def log_dump
    format="%5d %5d %5d %s\n"
    
    repo.log(1000).each do |log|
      puts "#{log.author.name} #{log.author.email} #{log.author.date}"
      diff = log.diff_parent.stats
      printf format, diff[:total][:insertions], diff[:total][:deletions], diff[:total][:lines], "total"
      diff[:files].each do |file,stats|
        printf "%5d %5d %5s %s\n", stats[:insertions], stats[:deletions], "", file
      end
    end
  end
end

if __FILE__ == $0
  puts "Run me!"

  ge = GitExplorer.new( 'ruby-git', 'https://github.com/ruby-git/ruby-git' )
  
  ge.log_dump
end

=begin
work_dir = ENV['WORK_DIR']
remote = ENV['REMOTE'] || 'https://github.com/ruby-git/ruby-git'

puts "Cloning #{remote} into #{work_dir}"

repo = Git.clone remote, "test", path: work_dir

=end
