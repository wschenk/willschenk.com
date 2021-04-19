require 'sqlite3'

def create_database filename
  db = SQLite3::Database.new filename

  rows = db.execute <<-SQL1
       CREATE TABLE IF NOT EXISTS authors (
              email text unique,
              name text,
              total_commits integer,
              total_tags integer default 0,
              earliest DATETIME,
              latest DATETIME,
              timezone_offset text
              );
SQL1
  rows = db.execute( "DELETE FROM authors;" );
  return db;
end

db = create_database "commits.db"
db.transaction

# Preload the tag counts
tag_counts = {}
db.execute(
  "select commits.author_email, count(name) from tags, commits where id=object or id=sha group by commits.author_email"
).each do |row|
  tag_counts[row[0]] = row[1]
end

db.execute( "select
    author_email,
    author_name,
    count(*) as cnt,
    min(author_when) as earliest,
    max(author_when) as latest
  from commits
  group by author_email
  order by count(*) desc" ).each do |row|
    email, name, cnt, earliest, latest = row
    tag_count = tag_counts[email] || 0

    ending = earliest[-6..-1] || earliest

    timezone_offset = ""
    if( ending =~ /[-+]\d\d:\d\d/ )
      timezone_offset = ending
    end

    db.execute( "INSERT INTO authors 
                (email,name,total_commits,total_tags,earliest,latest,timezone_offset) 
                values 
                (?,?,?,?,?,?,?)", [email, name, cnt, tag_count, earliest, latest, timezone_offset] );
end

db.commit
