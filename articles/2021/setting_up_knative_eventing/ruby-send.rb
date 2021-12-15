require 'sinatra'
require "cloud_events"
require "net/http"
require "uri"

def send_message( data )
  event = CloudEvents::Event.create spec_version:      "1.0",
                                    id:                "1234-1234-1234",
                                    source:            "/mycontext",
                                    type:              "com.example.someevent",
                                    data_content_type: "application/json",
                                    data:              data

  cloud_events_http = CloudEvents::HttpBinding.default
  headers, body = cloud_events_http.encode_event event
  Net::HTTP.post URI(ENV['K_SINK']), body, headers
end

get '/' do
  if params[:message]
    if ENV['K_SINK']
      send_message( {message: params[:message] })
      return "sent"
    else
      return "K_SINK not defined"
    end
  else
    return 'Try passing in a message'
  end
end


get '/keys' do
  ENV.keys.collect { |key| "#{key}=#{ENV[key]}" }.join( "\n" )
end
