require 'net/ssh'

host=ENV['SSH_HOST'] || 'apple.willschenk.com'

puts "Trying to connect to root@#{host}"
Net::SSH.start(host, 'root', timeout: 5 ) do |ssh|
  s = ssh.exec! "notdocker -v"
  puts s
  if s.exitstatus == 0
    puts "Success"
  else
    puts "Error"
  end

  s = ssh.exec! "docker -v"
  puts s
  if s.exitstatus == 0
    puts "Success"
  else
    puts "Error"
  end
end
