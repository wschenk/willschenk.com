require 'csv'
require 'date'
require 'json'

CONTRIBUTOR_ACTIVE_MONTHS = 3
PROJECT_IDLE_MONTHS = 6

author_last_seen = {}
author_commits = {}
period_commits = 0
period_authors = {}
period_author_first_seen = {}
tag_authors = {}
csv = []

last_date = nil
File.readlines( ARGV[0] || "commits.log" ).each do |line|
  date_string, email, name, decorator = line.split( "|" )
  date = Date.parse( date_string )

  csv << [date, 'project', 'project_started', ''] if last_date.nil?

  if last_date && Date.parse(last_date) >> PROJECT_IDLE_MONTHS < date
    csv << [Date.parse(last_date) >> PROJECT_IDLE_MONTHS, 'project', 'project_idle', period_authors.to_json, period_commits]
    csv << [date, 'project', 'project_active', '']
    period_commits = 0
    period_authors = {}
  end

  csv << [date, 'committer', 'started_contributing', name] if !author_last_seen[name]
  author_last_seen[name] = date
  author_commits[name] ||= 0
  author_commits[name] += 1
  period_authors[name] ||=0
  period_authors[name] += 1
  period_author_first_seen[name] ||= date
  tag_authors[name] ||= 0
  tag_authors[name] += 1

  left_authors = []
  author_last_seen.each do |author,last_seen|
    if last_seen >> CONTRIBUTOR_ACTIVE_MONTHS < date
      left_authors << author
      csv << [last_seen >> CONTRIBUTOR_ACTIVE_MONTHS, 'committer', 'left_project', author, author_commits[author]]
      csv << [period_author_first_seen[author], 'committer', 'activity_between', author, author_last_seen[author], author_commits[author]]
      author_commits.delete author
      period_author_first_seen.delete author
    end
  end
  left_authors.each { |author| author_last_seen.delete( author ) }

  # Add tagging event
  if decorator =~ /tag: (.*)/
    csv << [date, 'project', 'tag', $1, name, tag_authors.to_json]
    tag_authors = {}
  end

  last_date = date_string
  period_commits += 1
end

# Dump out remaining active people

period_author_first_seen.each do |author,first_seen|
  csv << [first_seen, 'committer', 'activity_between', author, author_last_seen[author], author_commits[author]]
end

csv << [Date.parse(last_date), 'project', 'last_data', author_commits.to_json, period_commits]

CSV.open( ARGV[1] || "project_timeline.csv", "w" ) do |out|
  out << ['date','actor', 'action', 'data', 'commits']

  csv.sort { |a,b| a[0] <=> b[0] }.each { |x| out << x }
end
