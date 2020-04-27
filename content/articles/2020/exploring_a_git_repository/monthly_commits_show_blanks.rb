require 'csv'
require 'date'
require 'json'

count = 0
last_month = nil
people = {}

CSV.open( ARGV[1] || "monthly_commits_show_blanks.csv", "w" ) do |csv|
  csv << ['date', 'commits', 'authors', 'authors_json' ]

  File.readlines( ARGV[0] || "commits.log" ).each do |line|
    date, email, name = line.split( "|" )
    month = date.gsub( /-\d\dT.*/, "" ).chomp;
    if( month != last_month && last_month )
      csv << [last_month, count, people.length, people.to_json]

      count = 0
      people = {}
      
      lc = 0
      
      # Fill in the missing months with zeros
      next_month = (Date.parse( "#{last_month}-01" ) >> 1).strftime( "%Y-%m" )
      while next_month != month
        csv << [next_month, 0, 0, {}.to_json]
        next_month = (Date.parse( "#{next_month}-01" ) >> 1).strftime( "%Y-%m" )
        lc += 1
        continue if lc > 24 # after 2 years this is probably broken
      end
    end
    
    people[name] ||= 0
    people[name] += 1
    
    count += 1
    last_month = month
  end
  
  csv << [ last_month, count, people.length, people.to_json ]
end
