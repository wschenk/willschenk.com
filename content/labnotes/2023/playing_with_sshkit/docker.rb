require 'dotenv/load'
require 'sshkit'
require 'sshkit/dsl'

SSHKit.config.output_verbosity = Logger::DEBUG

class Docker
  include SSHKit::DSL

  def affirm
    on( ENV['REMOTE_SERVER'] ) do |host|
      puts "Affirming docker is installed on #{host}"
      unless test( "docker -v" )
        execute( "curl -fsSL https://get.docker.com | sh" )
      end
    end
  end
end

if __FILE__ == $0
  docker_installed = Docker.new.affirm

  puts "Docker Installed: #{docker_installed}"
end
