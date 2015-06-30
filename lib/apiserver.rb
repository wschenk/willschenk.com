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

  post '/post' do
    puts "Saving #{params[:path]}"

    file = load_app.sitemap.find_resource_by_path params[:path]

    File.open( file.source_file, "w" ) do |out|
      out.puts YAML.dump( params[:meta] )
      out.puts "---"
      out.puts params[:body]
    end

    "Saved your draft!"
  end

  post '/images' do
    p params

    File.open('/tmp/' + params['file'][:filename], "wb") do |f|
      f.write(params['file'][:tempfile].read)
    end

    "Thanks"
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