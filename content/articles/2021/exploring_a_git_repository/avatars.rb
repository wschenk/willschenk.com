require 'fileutils'
require 'digest'

FileUtils.mkdir_p "avatars"

File.readlines( "authors.log" ).each do |line|
  email,name = line.chomp.split( /\:/ )
  if( !File.exists?( "avatars/#{name}.png" ) )
    md5 = Digest::MD5.hexdigest email
    system( "curl http://www.gravatar.com/avatar/#{md5}?size=512 -o \"avatars/#{name}.png\"" )
  else
    puts "#{name}.png already exists"
  end
end
