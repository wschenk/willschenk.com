require 'sqlite3'
require 'csv'
require 'semver'

def create_commits_table db
  db.execute <<-SQL1
       CREATE TABLE IF NOT EXISTS commits (
              id	TEXT UNIQUE,
              tag TEXT,
              summary	TEXT,
              author_name	TEXT,
              author_email	TEXT,
              author_when	DATETIME
              );
SQL1

  db.execute <<-SQL2
       CREATE TABLE IF NOT EXISTS commit_files (
              id    TEXT,
              name  TEXT,
              added INT,
              deleted INT
              );
SQL2
end

def create_tags_table db
  db.execute <<-SQL3
       CREATE TABLE IF NOT EXISTS tags (
              name TEXT UNIQUE,
              sha TEXT,
              object TEXT,
              author_email TEXT,
              major_version INTEGER,
              minor_version INTEGER,
              patch_version INTEGER,
              rc_version INTEGER,
              created_at DATETIME
              );
SQL3
end

def create_authors_table db
  db.execute <<-SQL4
         CREATE TABLE IF NOT EXISTS authors (
                email text unique,
                name text,
                total_commits integer,
                total_tags integer default 0,
                earliest DATETIME,
                latest DATETIME,
                timezone_offset text
                );
SQL4
end

def create_file_stats_table db
  db.execute <<-SQL5
         CREATE TABLE IF NOT EXISTS file_stats_summary (
                sha text,
                tag text,
                author_name text,
                author_email text,
                author_when text,
                code_lines integer,
                comment_lines integer,
                blank_lines integer
                );
SQL5

  db.execute <<-SQL6
         CREATE TABLE IF NOT EXISTS file_stats (
                sha text,
                filename text,
                language text,
                code_lines integer,
                comment_lines integer,
                blank_lines integer
                );
SQL6
end

def create_timeline_table db
  db.execute <<-SQL7
       CREATE TABLE IF NOT EXISTS timeline (
              event_at DATETIME,
              event_verb TEXT,
              event_author TEXT,
              event_subject TEXT,
              concurrent_contributors INTEGER,
              commits INTEGER,
              entities_changed INTEGER
              );
SQL7
  db.execute( "DELETE FROM timeline;" );
end

repo_dir = ENV['REPO_DIR'] || "."
database = File.join( ENV['OUTPUT_DIR'] || ".", "repository.sqlite" )

puts "Repodir: #{repo_dir}"

if !File.exists?( File.join( repo_dir, ".git" ) )
  if !ENV['REPO_URL']
    puts "Couldn't find a repo at #{repo_dir} and REPO_URL is unset"
    exit 1
  end
  puts "Cloning repo #{ENV['REPO_URL']} into #{repo_dir}"
  value = system( "git clone #{ENV['REPO_URL']} #{repo_dir}" )
end

head = `(cd #{repo_dir};git rev-parse HEAD)`.chomp

db = SQLite3::Database.new database
create_commits_table db
create_tags_table db
create_authors_table db
create_file_stats_table db
create_timeline_table db

def add_commit db, id, email, name, date, summary
  ret = db.execute("INSERT INTO commits (id, summary, author_name, author_email, author_when)
        VALUES (?, ?, ?, ?, ?)", [id, summary, name, email, date ] )
end

def add_file_commit db, id, file, added, deleted
  ret = db.execute("INSERT INTO commit_files (id, name, added, deleted)
        VALUES (?, ?, ?, ?)", [id, file, added, deleted] )

end

def load_commits db, repo_dir
  commit = nil

  `(cd #{repo_dir};git log --pretty=format:'|%H|%ae|%an|%aI|%s' --numstat)`.each_line do |line|
    line.chomp!
    if line[0] == '|'
      md = /\|(.*?)\|(.*?)\|(.*?)\|(.*?)\|(.*)/.match( line )
      commit = md[1]

      puts line

      begin
        add_commit db, md[1], md[2], md[3], md[4], md[5]
      rescue SQLite3::ConstraintException
        puts "Found existing commit, exiting"
        return
      end
    elsif line.length != 0
      md = /([\d|-]*)\s*([\d|-]*)\s*(.*)/.match( line )
      add_file_commit db, commit, md[3], md[1], md[2]
    end
  end
end

puts "Finding commits"
db.transaction
load_commits db, repo_dir
db.commit

# figure out the major/minor

def guess_version tag_name
  version = SemVer.parse_rubygems tag_name

  return { major_version: version.major,
    minor_version: version.minor,
    patch_version: version.patch,
    rc_version: version.special }
end


# Insert into the database
def add_tag db, name, sha, object, created_at, author_email
  version = guess_version name

  ret = db.execute("INSERT INTO tags
(name, sha, object, created_at, author_email, 
major_version, minor_version, patch_version, rc_version)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                   [name, sha, object, created_at, author_email,
                    version[:major_version], version[:minor_version],
                    version[:patch_version], version[:rc_version]])
end

# Parse the output of git tag
def load_tags db, repo_dir
  `(cd #{repo_dir};git tag --sort=-v:refname --format='%(refname:short):%(objectname):%(*objectname):%(creatordate:iso8601-strict)')`.each_line do |line|
    line.chomp!
    md = line.match( /(.*?):(.*?):(.*?):(.*)/ )

    if( !md )
        puts "Unrecognized line #{line}"
    else
      tag_name = md[1]
      sha = md[2]
      sha_object = md[3] unless md[3] == "" # have annotated tags point to the main commit
      created_at = md[4]

      tagger = db.execute( "select author_email from commits where id = (?) or id = (?)", [sha, sha_object == "" ? sha : sha_object] ).first

      begin
        add_tag db, tag_name, sha, sha_object, created_at, tagger
      rescue SQLite3::ConstraintException
        puts "Found existing tag, exiting"
        return
      end
    end
  end
end

# Match up the commits to the tags for easier querying
def tag_commits db
  last_date = nil
  db.execute( "select name, created_at from tags where created_at is not null order by created_at asc" ).each do |row|
    if last_date.nil?
      db.execute( "update commits set tag = (?) where author_when <= (?)", [row[0], row[1]] )
    else
      db.execute( "update commits set tag = (?) where author_when <= (?) and author_when > (?)", [row[0], row[1], last_date] )
    end

    last_date = row[1]
  end

  db.execute( "update commits set tag = 'HEAD' where author_when > (?)", [last_date] )
end

puts "Finding tags"
db.transaction
db.execute( "delete from tags" );
load_tags db, repo_dir
tag_commits db
db.commit

db.transaction

puts "Summarizing authors"
db.execute( "delete from authors" );

db.execute( "   select
     author_name,
     author_email,
     count(commits.id) as commit_count,
     count(distinct(tag)) as tag_count,
     min( author_when ) as first_contrib,
     max( author_when ) as last_contrib
  from commits
  group by author_email" ).each do |row|
    name, email, cnt, tag_count, earliest, latest = row

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

def import_cloc_output repo_dir, db, sha
  puts "Finding stats for #{sha}"
  output = `(cd #{repo_dir};cloc --skip-uniqueness --quiet --by-file --csv --git #{sha})`
  CSV.parse(output).each do |row|
    next if row == []
    next if row[0] == 'language'
    if row[0] == 'SUM'
      return { code_lines: row[4], comment_lines: row[3], blank_lines: row[2] }
    else
      ret = db.execute(
        "INSERT INTO file_stats (sha, filename, language, code_lines, comment_lines, blank_lines)
        VALUES (?, ?, ?, ?, ?, ?)",
        sha, row[1], row[0], row[4], row[3], row[2] )
    end
  end
end

def find_commit db, sha, obj
  row = db.execute( "select author_name, author_email, author_when from commits where id=(?) or id=(?)", [sha, obj] ).first

  return nil if row.nil?
  return {author_name: row[0], author_email: row[1], author_when: row[2]}
end

def add_summary db, summary, name, sha, commit
  commit ||= {}
  author_name = commit[:author_name]
  author_email = commit[:author_email]
  author_when = commit[:author_when];

  ret = db.execute(
    "INSERT INTO file_stats_summary (sha, tag, author_name, author_email, author_when, code_lines, comment_lines, blank_lines)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    [sha,
     name,
     author_name,
     author_email,
     author_when,
     summary[:code_lines],
     summary[:comment_lines],
     summary[:blank_lines]
    ])
end

db.transaction
summary = import_cloc_output repo_dir, db, head
add_summary db, summary, "HEAD", head, find_commit( db, head, head )

db.execute( "select sha, name, object from tags" ) do |row|
  sha = row[0]
  name = row[1]
  object = row[2]
  summary = import_cloc_output repo_dir, db, sha
  add_summary db, summary, name, sha, find_commit( db, sha, object );
end
db.commit

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

db.transaction

project_start_stop db
contributors db
releases db

db.commit
