require 'json'

trips = JSON.parse( File.read( 'trips.json' ))

puts "Shapes in trips"
shapes = {}
trips.each do |t|
  shapes[t['shape_id'].to_i] ||= 0
  shapes[t['shape_id'].to_i] += 1
end

shapes.keys.sort.each do |k|
  puts "Shape #{k}: #{shapes[k]} trips"
end
  

trips.select { |t| t['shape_id'] == '50' }.each do |trip|
  printf "%10s %20s\n", trip['trip_headsign'], trip['service_id']
end
