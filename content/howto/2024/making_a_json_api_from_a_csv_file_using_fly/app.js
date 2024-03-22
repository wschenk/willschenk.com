require 'sinatra/base'
require 'sqlite3'
require "sinatra/activerecord"

class App < Sinatra::Base
  register Sinatra::ActiveRecordExtension
  
  set :database, {adapter: "sqlite3", database: "stations.db"}
  
  get '/' do
    "Hello world"
  end
end
