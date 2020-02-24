require 'csv'
require 'date'
require 'json'

author_last_seen = {}
author_commits = {}
last_month = nil
period_commits = 0

CSV.open( "project_timeline.csv", "w" ) do |csv|
  csv << ['date','actor','id','action','commits']

  last_date = nil
  File.readlines( "authors.log" ).each do |line|
    date_string, email, name = line.split( "|" )
    month = date_string.gsub( /-\d\dT.*/, "" ).chomp
    date = Date.parse( date_string )

    
    csv << [date, 'project', '', 'project_started'] if last_date.nil?

    csv << [date, 'committer', name, 'started_contributing'] if !author_last_seen[name]
    author_last_seen[name] = date
    author_commits[name] ||= 0
    author_commits[name] += 1

    left_authors = []
    author_last_seen.each do |author,last_seen|
      if last_seen >> 3 < date
        left_authors << author
        csv << [last_seen >> 3, 'committer', author, 'left_project', author_commits[author]]
        author_commits[author] = 0
      end
    end
    left_authors.each { |author| author_last_seen.delete( author ) }

    
    if last_date && Date.parse(last_date) >> 6 < date
      csv << [Date.parse(last_date), 'project', '', 'project_idle', period_commits]
      csv << [date, 'project', '', 'project_active']
      period_commits = 0
    end

    last_date = date_string
    period_commits += 1
  end

  csv << [Date.parse(last_date), 'project', '', 'last_data', period_commits]
end
