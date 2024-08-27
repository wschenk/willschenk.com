require 'dotenv/load'
require 'sequel'

throw "DATABASE_URL is unset" if !ENV['DATABASE_URL'] || ENV['DATABASE_URL'] == ""

DB = Sequel.connect( ENV['DATABASE_URL'] )

Sequel::Model.plugin :timestamps, update_on_create: true
