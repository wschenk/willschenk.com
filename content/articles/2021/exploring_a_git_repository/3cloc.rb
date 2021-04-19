require 'sqlite3'
require 'csv'
exit
def create_database filename
  db = SQLite3::Database.new filename

  rows = db.execute <<-SQL1
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
SQL1
  rows = db.execute "delete from file_stats_summary";

  rows = db.execute <<-SQL1
       CREATE TABLE IF NOT EXISTS file_stats (
              sha text,
              filename text,
              language text,
              code_lines integer,
              comment_lines integer,
              blank_lines integer
              );
SQL1
  rows = db.execute "delete from file_stats";
  return db;
end

def import_cloc_output db, sha
  puts "Finding stats for #{sha}"
  output = `(cd #{ENV['REPO_WORK_DIR']};cloc --skip-uniqueness --quiet --by-file --csv --git #{sha})`
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

db = create_database "commits.db"

db.transaction
head = `(cd #{ENV['REPO_WORK_DIR']};git rev-parse HEAD)`.chomp
summary = import_cloc_output db, head
add_summary db, summary, "HEAD", head, find_commit( db, head, head )

db.execute( "select sha, name, object from tags" ) do |row|
  sha = row[0]
  name = row[1]
  object = row[2]
  summary = import_cloc_output db, sha
  add_summary db, summary, name, sha, find_commit( db, sha, object );
end
db.commit
