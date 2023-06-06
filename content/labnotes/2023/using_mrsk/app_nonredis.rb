require 'sinatra/base'

class App < Sinatra::Base
  get '/' do
    "Hello world!!!"
  end

  get '/up' do
    "Yup"
  end
end
