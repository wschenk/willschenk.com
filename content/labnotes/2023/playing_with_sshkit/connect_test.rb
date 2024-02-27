require 'dotenv/load'
require 'sshkit'
require 'sshkit/dsl'
include SSHKit::DSL

host = ENV['REMOTE_SERVER']

on( host ) do
  puts capture( 'hostname' )
  puts capture( 'pwd' )
end
