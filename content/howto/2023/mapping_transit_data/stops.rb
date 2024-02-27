#!/usr/bin/env ruby

require 'json'

shapes = JSON.parse( File.read( 'shapes.json' ) ).group_by { |s| s['shape_id'] }
stops = JSON.parse( File.read( 'stops.json' ) )

puts "There are #{shapes.length} different shapes"

# I'm just picking on here to limit the output
shape = shapes['50']
stops.each do |stop|
  shape.each do |point|
    if point['shape_pt_lat'] == stop['stop_lat'] &&
       point['shape_pt_lon'] == stop['stop_lon']
      puts stop['stop_name']
    end
  end
end
