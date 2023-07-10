require 'net/scp'
host=ENV['SSH_HOST'] || 'apple.willschenk.com'

puts "Trying to connect to root@#{host}"
Net::SSH.start(host, 'root', timeout: 5 ) do |ssh|
  s = ssh.exec! "ls /tmp/copy_file.rb"

  if s.exitstatus == 0
    puts "File exists"
  else
    puts "File doesn't exist"
  end

  puts "Uploading file"

  ssh.scp.upload! "copy_file.rb", "/tmp/copy_file.rb"

  s = ssh.exec! "ls /tmp/copy_file.rb"

  if s.exitstatus == 0
    puts "File exists"
  else
    puts "File doesn't exist"
  end
end
