require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'sqlite3'
end

db = SQLite3::Database.open 'stations.db'

i = 0
results = db.query( "select distinct [EV Connector Types]
        from stations where [EV Connector Types] != ''" )

results.each do |r|
  fields = r[0].split(' ' ).collect { |x| "#{x} = 1 "}.join(" and ")
  cmd =  "update stations set #{fields} where [EV Connector Types] = '#{r[0]}'"
  puts cmd
  db.execute cmd
  
  i = i + 1
  if i % 1000 == 0
    puts "#{i} rows processed"
  end
end
