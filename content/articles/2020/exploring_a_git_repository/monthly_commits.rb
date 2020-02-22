require 'date'
require 'json'

count = 0
last_month = nil
people = {}

puts "date\tcommits\tauthors\tauthor_details"
File.readlines( "authors.log" ).each do |line|
  date, email, name = line.split( "|" )
  month = date.gsub( /-\d\dT.*/, "" ).chomp

  if month != last_month && last_month
    printf "%s\t%s\t%s\t%s\n", last_month, count, people.length, people.to_json

    count = 0
    people = {}
  end

  people[name] ||= 0
  people[name] += 1

  count += 1
  last_month = month
end

printf "%s\t%s\t%s\t%s\n", last_month, count, people.length, people.to_json
