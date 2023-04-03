require 'sqlite3'

def create_database filename
  db = SQLite3::Database.new filename

  rows = db.execute <<-SQL1
       CREATE TABLE IF NOT EXISTS timeline (
              event_at DATETIME,
              event_verb TEXT,
              event_author TEXT,
              event_subject TEXT,
              concurrent_contributors INTEGER,
              commits INTEGER,
              entities_changed INTEGER
              );
SQL1
  rows = db.execute( "DELETE FROM timeline;" );
  return db;
end

def concurrent_contribs db, at
  return db.execute( "select count(*) from authors where earliest <= (?) and date(latest,'+90 days') >= (?)", [at, at]).first
end

def add_event db, at, verb, author, subject
  concurrent = concurrent_contribs db, at

  commits = db.execute( "select count(*) from commits where author_email = (?)", [author] ).first
  entities_changed = db.execute( "select count(distinct(name)) from commits, commit_files where commits.id = commit_files.id and author_email = (?)", [author]).first

  db.execute( "INSERT INTO timeline (event_at, event_verb, event_author, event_subject, concurrent_contributors, commits, entities_changed)
  VALUES (?, ?, ?, ?, ?, ?, ?)",
              [at, verb, author, subject, concurrent, commits, entities_changed] );
end

def project_start_stop db
  row = db.execute( "select author_name, author_email, author_when from commits order by author_when asc limit 1" ).first

  add_event db, row[2], "project.start", row[1], "#{row[0]} made first commit"

  row = db.execute( "select author_name, author_email, author_when from commits order by author_when desc limit 1" ).first

  add_event db, row[2], "project.mostrecent", row[1], "#{row[0]} made most recent"
end

def contributors db
  # Look through all the authors to add when the started and stopped.
  rows= db.execute( "select email, name, earliest, latest from authors" )
  rows.each do |row|
    if row[2] != row[3]
      add_event db, row[2], "contrib.start", row[0], row[1]
      # only add latest if it was 45 at least 45 days ago
      add_event db, row[3], "contrib.latest", row[0], row[1]
    end
  end
end

def releases db
  db.execute( "
  select tag,
  count(distinct(commits.author_email)) as contributors,
  count(*) as commits,
  count(distinct(commit_files.name)) as entities,
  tags.name,
  tags.author_email,
  max(author_when)
  from commits, commit_files
  left join tags on tags.name = commits.tag
  where commits.id = commit_files.id 
  group by tag
  order by max(author_when)
" ).each do |row|
    tag, contributors, commits, entities, name, author_email, author_when = row

    db.execute( "INSERT INTO timeline 
(event_at, event_verb, event_author, event_subject, concurrent_contributors, entities_changed, commits)
VALUES
(?, ?, ?, ?, ?, ?, ?)",
                [author_when, 'project.release', author_email, tag, contributors, entities, commits] )
  end
end

db = create_database "commits.db"
db.transaction

project_start_stop db
contributors db
releases db

db.commit
