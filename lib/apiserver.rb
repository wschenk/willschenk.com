require 'sinatra/base'
require 'sinatra/json'

class ApiServer < Sinatra::Base
  get '/' do
    "This is the api server"
  end

  get '/post' do
    app = load_app

    logger.info "Looking up #{params[:path]}"

    file = app.sitemap.find_resource_by_path params[:path] if params[:path]

    if !file
      status 404
      json error: "Unknown path #{params[:path]}"
    else
      raw = File.read file.source_file
      body = raw.gsub( /^---\n.*?---\n*/m, "" ) # Remove the preyaml

      json meta: file.data, content: body
    end
  end

  post '/post' do
    logger.info "Saving #{params[:path]}"

    file = load_app.sitemap.find_resource_by_path params[:path]

    if !file
      logger.info "Unknown path: #{params[:path]}"
      status 404
      json error: "Unknown path #{params[:path]}"
    else
      File.open( file.source_file, "w" ) do |out|
        out.puts YAML.dump( params[:meta] )
        out.puts "---"
        out.puts params[:body]
      end
    end

    json message: "Post saved"
  end

  post '/images' do
    p params

    File.open('/tmp/' + params['file'][:filename], "wb") do |f|
      f.write(params['file'][:tempfile].read)
    end

    json message: "Image uploaded"
  end

  post '/build' do
    `bundle exec middleman build 2>&1`
  end

  post '/update' do
    `git pull origin master 2>&1`
  end

  post '/status' do
    `git status 2>&1`
  end

  post '/drafts' do
    p params

    if !params[:title]
      status 404
      json error: "Bad Parameters"
    else
      logger.info "Created draft for #{params[:title]}"

      slug = params[:title].downcase.gsub( /[^a-z]/, "-" )

      outfile = File.expand_path( "source/drafts/#{slug}.html.markdown", Dir.pwd )

      File.open( outfile, "w" ) do |out|
        out.puts YAML.dump( params )
        out.puts "---\n\n# #{params[:title]}\nHere we go!"
      end

      json created: slug
    end
  end

  private
  def load_app
    opts = {}

    app = ::Middleman::Application.server.inst do
      set :environment, opts[:environment].to_sym if opts[:environment]

      # ::Middleman::Logger.singleton(opts[:debug] ? 0 : 1, opts[:instrumenting] || false)
      logger
    end

    app
  end

  def logger
    ::Middleman::Logger.singleton( 1 )
  end
end