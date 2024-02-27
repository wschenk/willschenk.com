require 'dotenv/load'
require 'sshkit'
require 'sshkit/dsl'

#SSHKit.config.output_verbosity = Logger::DEBUG

class AptPackages
  include SSHKit::DSL

  def affirm( *package_list )
    on( ENV['REMOTE_SERVER'] ) do |host|
      install_list = []
      package_list.each do |p|
        puts "Affirming #{p} is installed on #{host}"
        unless test( "which #{p}" )
          puts "#{p} not found"
          install_list << p
        end
      end

      if install_list.length > 0
        execute( "apt-get update" )
        execute( "apt-get install -y #{install_list.join( " " )}")
      end
    end
  end
end

if __FILE__ == $0
  AptPackages.new.affirm( "jq", "jo", "zsh", "git" )
end
