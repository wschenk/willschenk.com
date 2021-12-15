require 'sinatra'
require "cloud_events"

class EventHolder
  def self.add_event( e )
    @events ||= []
    @events << e.to_h
  end

  def self.eventstring
    @events ||= []
    @events.join( "\n" )
  end
end

get '/' do
  "Hello from the ruby echo service\n#{EventHolder.eventstring}\n"
end

cloud_events_http = CloudEvents::HttpBinding.default

post "/" do
  event = cloud_events_http.decode_event request.env
  logger.info "Received CloudEvent: #{event.to_h}"
  EventHolder.add_event( event )
end
