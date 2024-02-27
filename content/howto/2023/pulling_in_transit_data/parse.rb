#!/usr/bin/env ruby

require 'zip'
require 'json'
require 'csv'

STATION_INFO_URL="http://web.mta.info/developers/data/mnr/google_transit.zip"

def ensure_google_transit
  if !File.exist? "google_transit.zip"
    system( "curl -O #{STATION_INFO_URL}" )
  end
end

def write_table csv_data, outfile
  table = CSV.parse csv_data, headers: true
  p table.headers
  ret = []
  table.each do |r|
    d = {}
    table.headers.each do |h|
      d[h] = r[h]
      case h
      when 'stop_lon', 'stop_lat', 'shape_pt_lat', 'shape_pt_lon', 'shape_pt_sequence'
        d[h] = d[h].to_f
      end
    end

    ret << d
  end

  puts "Writing #{outfile}"
  File.open( outfile, "w" ) do |out|
    out << JSON.pretty_generate( ret )
  end
end

ensure_google_transit

Zip::File.open( 'google_transit.zip' ) do |zipfile|
  write_table( zipfile.read( 'stops.txt' ), 'stops.json' )
  write_table( zipfile.read( 'routes.txt' ), 'routes.json' )
  write_table( zipfile.read( 'shapes.txt' ), 'shapes.json' )
  write_table( zipfile.read( 'trips.txt' ), 'trips.json' )
end
