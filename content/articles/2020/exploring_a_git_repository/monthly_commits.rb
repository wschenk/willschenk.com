require 'csv'
require 'json'

count = 0
last_month = nil
people = {}

CSV.open( "monthly_commits.csv", "w" ) do |csv|
  csv << ['date', 'commits', 'authors', 'authors_json' ]

  File.readlines( "authors.log" ).each do |line|
    date, email, name = line.split( "|" )
    month = date.gsub( /-\d\dT.*/, "" ).chomp

    if month != last_month && last_month
      csv << [last_month, count, people.length, people.to_json]

      count = 0
      people = {}
    end
    
    people[name] ||= 0
    people[name] += 1
    
    count += 1
    last_month = month
  end

  csv << [last_month, count, people.length, people.to_json]
end
