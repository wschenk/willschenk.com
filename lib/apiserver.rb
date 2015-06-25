require 'sinatra'

class ApiServer < Sinatra::Base
  get '/' do
    "This is the api server"
  end

  get '/post' do
    app = load_app

    puts "Lookig up #{params[:path]}"
    file = app.sitemap.find_resource_by_path params[:path]

    raw = File.read file.source_file
    body = raw.gsub( /^---\n.*?---\n*/m, "" ) # Remove the preyaml

    { meta: file.data, content: body }.to_json
  end

  put '/build' do
    system( "bundle exec middleman build")
  end

  private
  def load_app
    opts = {}

    app = ::Middleman::Application.server.inst do
      set :environment, opts[:environment].to_sym if opts[:environment]

      ::Middleman::Logger.singleton(opts[:debug] ? 0 : 1, opts[:instrumenting] || false)
    end

    app
  end
end