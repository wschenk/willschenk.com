require 'net/ssh'

host=ENV['SSH_HOST'] || 'apple.willschenk.com'

puts "Trying to connect to root@#{host}"
Net::SSH.start(host, 'root', timeout: 5 ) do |ssh|
  puts ssh.exec! "hostname"
  puts ssh.exec! "uname -a"
end
