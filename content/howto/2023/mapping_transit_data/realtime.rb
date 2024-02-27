require 'dotenv/load'
require 'protobuf'
require 'google/transit/gtfs-realtime.pb'
require 'net/http'
require 'uri'

url='https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/mnr%2Fgtfs-mnr'

response = Net::HTTP.get_response(URI(url), { 'x-api-key' => ENV['API_KEY'] } )
data = response.body
feed = Transit_realtime::FeedMessage.decode(data)
puts JSON.pretty_generate(feed)
exit
for entity in feed.entity do
  if entity.field?(:trip_update)
    p entity.trip_update
    exit
  end
end
