require 'net/scp'
host=ENV['SSH_HOST'] || 'apple.willschenk.com'

user = "gituser"
puts "Trying to connect to root@#{host}"
Net::SSH.start(host, 'root', timeout: 5 ) do |ssh|
  puts "Creating #{user}"

  res = ssh.exec! "useradd #{user} -m -g docker -s /bin/bash"

  puts res

  res = ssh.exec! "grep #{user} /etc/passwd"
  puts res

  if res.exitstatus != 0
    puts "Didn't create #{user}"
    exit
  end

  remote_home = res.split( /:/ )[5]

  res = ssh.exec! "mkdir -p #{remote_home}/.ssh"
  puts res
  if res.exitstatus != 0
    puts "Error creating directory"
    exit
  end

  puts "Uploading public key"
  ssh.scp.upload! "#{ENV['HOME']}/.ssh/id_ed25519.pub",
                  "#{remote_home}/.ssh/authorized_keys"

  puts "Fixing permissions"
  puts ssh.exec! "chown -R #{user} #{remote_home}/.ssh"
  puts ssh.exec! "chmod 700 #{remote_home}/.ssh"
  puts ssh.exec! "chmod 600 #{remote_home}/.ssh/authorized_keys"
end

require 'net/scp'
host=ENV['SSH_HOST'] || 'apple.willschenk.com'

user = "gituser"
puts "Trying to connect to #{user}@#{host}"
Net::SSH.start(host, user, timeout: 5 ) do |ssh|
  puts "Creating #{user}"

  puts ssh.exec! "whoami"
  puts ssh.exec! "pwd"
end
