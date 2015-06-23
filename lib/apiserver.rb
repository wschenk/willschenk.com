require 'sinatra'

class ApiServer < Sinatra::Base
  get '/' do
    "This is the api server"
  end

  get '/raw' do
    puts "Loading up raw file #{params[:file]}"
    File.read(File.expand_path(params[:file],"source"))
  end

  put '/build' do
    system( "bundle exec middleman build")
  end
end