require 'dotenv/load'
require 'sshkit'
require 'sshkit/dsl'

#SSHKit.config.output_verbosity = Logger::DEBUG

class CopyFiles
  include SSHKit::DSL

  def affirm( *file_list )
    on( ENV['REMOTE_SERVER'] ) do |host|
      file_list.each do |f|
        upload! f, f
      end
    end
  end
end

if __FILE__ == $0
  CopyFiles.new.affirm( "caddy-compose.yml", "whoami-compose.yml" )
end
