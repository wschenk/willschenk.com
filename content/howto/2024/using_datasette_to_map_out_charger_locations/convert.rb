require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'sqlite3'
end

db = SQLite3::Database.open 'stations.db'

# Journal mode for database, WAL=write-ahead log
db.execute 'PRAGMA main.journal_mode=WAL;'
# Storage location for temporary tables, indices, views, triggers
db.execute 'PRAGMA main.temp_store = MEMORY;'

i = 0
results = db.query( "select id, [EV Connector Types] from stations where [EV Connector Types] != ''" )
db.execute('BEGIN TRANSACTION')

results.each do |r|
  r[1].split( " " ).each do |type|
    db.execute( "update stations set #{type} = 1 where id = #{r[0]}" )
  end

  i = i + 1
  if i % 1000 == 0
    puts "#{i} rows processed"
    db.execute('END TRANSACTION')
    db.execute('BEGIN TRANSACTION')

  end
end
db.execute('END TRANSACTION')
