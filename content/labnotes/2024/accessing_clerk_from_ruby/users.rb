# users.rb
require 'dotenv/load'
require 'clerk'

clerk = Clerk::SDK.new

# List all users
clerk.users.all

# Get your first user
user = clerk.users.all(limit: 1).first

# Print it if you want
# require 'pp'
# pp user


# Look up a user given their id

puts "Looking up user #{user['id']}"
user = clerk.users.find( user['id'] )

puts user['email_addresses'].first['email_address']
