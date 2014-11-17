require 'middleman-core/preview_server'

class ApiServer < Sinatra::Base
  get '/' do
    "This is the api server"
  end

  get '/raw' do
    puts "Loading up raw file #{params[:file]}"
    File.read(File.expand_path(params[:file],"source"))
  end
end


module Middleman
  module PreviewServer
    class << self

      @@mount_instance_orig = Middleman::PreviewServer.method(:mount_instance)
      def mount_instance( app )
        server = ::Middleman::Application.server

        meta_app = ApiServer.new
        app.map '/api' do
          run meta_app
        end

        res = @@mount_instance_orig.call( app )
        logger.info "== Middleman API at http://#{host}:#{port}/api/"

        res
      end
    end
  end
end
#         # puts "Hi there"

#       #   puts app.class.to_s

#       #   @@mount_instance_orig.call(app)

#       #   require 'pp'
#       #   server = ::Middleman::Application.server


#       #   # Add in the meta pages application
#       #   meta_app = Middleman::ApiServer::Application.new(server)
#       #   server.map '/api' do
#       #     run meta_app
#       #   end
#       #   pp server
#       #   puts
#       #   pp server.map

#       #   exit
#       # end

#       @@new_app = Middleman::PreviewServer.method( :new_app )
#       def new_app
#         puts "Calling original new app"

#         res = @@new_app.call

#         puts "Called new app"

#         puts res.class

#         res
#       end
#     end
#   end
# end

