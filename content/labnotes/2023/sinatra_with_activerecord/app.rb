# app.rb
require 'sinatra'
require "sinatra/activerecord"
require_relative 'routes/posts.rb'
require_relative 'routes/account.rb'

# For cookies
use Rack::Session::Cookie, :key => 'rack.session',
    :path => '/',
    :secret => 'sosecret'

set :default_content_type, :json

get '/' do
  {message:"Hello world."}
end

get '/up' do
  {success:true}
end
