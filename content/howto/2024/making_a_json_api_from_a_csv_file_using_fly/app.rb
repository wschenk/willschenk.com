require 'sinatra/base'
require 'sqlite3'
require "sinatra/activerecord"
require_relative './loader'

class Data < ActiveRecord::Base
end

class App < Sinatra::Base
  register Sinatra::ActiveRecordExtension
  l = Loader.new
  
  set :database, {adapter: "sqlite3", database: l.db}
  
  get '/' do
    l = Loader.new
    content_type :json

    { db: l.db, csv: l.csv, csv_exists: l.csv_exists?, db_exists: l.db_exists? }.to_json
  end

  get '/stats' do
    content_type :json
    {
      count: Data.count,
      ct: Data.where( "State = ?", "CT" ).count,
      ny: Data.where( "State = ?", "NY" ).count
    }.to_json
  end

  post '/' do
    l = Loader.new

    l.ensure!

    redirect '/'
  end
end
