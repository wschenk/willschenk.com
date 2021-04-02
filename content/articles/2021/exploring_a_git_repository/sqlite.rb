require 'sqlite3'

def create_database filename
  db = SQLite3::Database.new filename

  rows = db.execute <<-SQL1
       CREATE TABLE IF NOT EXISTS commits (
              id	TEXT UNIQUE,
              summary	TEXT,
              author_name	TEXT,
              author_email	TEXT,
              author_when	DATETIME
              );
SQL1

  rows = db.execute <<-SQL2
       CREATE TABLE IF NOT EXISTS commit_files (
              id    TEXT,
              name  TEXT,
              added INT,
              deleted INT
              );
SQL2

  db
end

def add_commit db, id, email, name, date, summary
  ret = db.execute("INSERT INTO commits (id, summary, author_name, author_email, author_when)
        VALUES (?, ?, ?, ?, ?)", [id, summary, name, email, date ] )
end

def add_file_commit db, id, file, added, deleted
  ret = db.execute("INSERT INTO commit_files (id, name, added, deleted)
        VALUES (?, ?, ?, ?)", [id, file, added, deleted] )

end

def parse_file db, file
  commit = nil
  File.open( file ).each_line do |line|
    line.chomp!
    if line[0] == '|'
      md = /\|(.*?)\|(.*?)\|(.*?)\|(.*?)\|(.*)/.match( line )
      commit = md[1]

      puts line

      begin
        add_commit db, md[1], md[2], md[3], md[4], md[5]
      rescue SQLite3::ConstraintException
        puts "Found existing commit, exiting"
        exit
      end
    elsif line.length != 0
      md = /([\d|-]*)\s*([\d|-]*)\s*(.*)/.match( line )
      add_file_commit db, commit, md[3], md[1], md[2]
    end
  end
end

db = create_database "commits.db"
db.transaction
parse_file db, "commits_with_files.log"
db.commit
