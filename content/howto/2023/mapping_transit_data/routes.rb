#!/usr/bin/env ruby

require 'json'

routes = JSON.parse( File.read( 'routes.json' ))
trips = JSON.parse( File.read( 'trips.json' ))

routes.each do |route|
  printf "%5s %10s\n", route["route_id"], route["route_long_name"]

  t = trips.select { |t| t['route_id'] == route['route_id'] }
  printf "%5s %10s\n\n", t.length, "trips"
end
