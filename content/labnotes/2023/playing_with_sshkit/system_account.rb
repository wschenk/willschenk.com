require 'dotenv/load'
require 'sshkit'
require 'sshkit/dsl'

#SSHKit.config.output_verbosity = Logger::DEBUG

class SystemAccount
  include SSHKit::DSL

  def affirm( user, shell )
    on( ENV['REMOTE_SERVER'] ) do |host|
      puts capture( "which #{shell}" )
    end
  end
end

if __FILE__ == $0
  SystemAccount.new.affirm( ENV['USER'], 'bash' )
  SystemAccount.new.affirm( 'git', 'git-shell' )
end
