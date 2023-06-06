require 'sinatra/base'
require 'redis'

class App < Sinatra::Base
  get '/' do
    redis = Redis.new( url: ENV['REDIS_URL'] )
    count = redis.incr("counter")
    "Hello world, called #{count} times"
  end

  get '/up' do
    "Yup"
  end
end
