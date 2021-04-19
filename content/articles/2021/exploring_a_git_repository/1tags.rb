require 'sqlite3'

def create_database filename
  db = SQLite3::Database.new filename

  rows = db.execute <<-SQL1
       CREATE TABLE IF NOT EXISTS tags (
              name TEXT UNIQUE,
              sha TEXT,
              object TEXT,
              author_email TEXT,
              created_at DATETIME
              );
SQL1

  return db
end

def add_tag db, name, sha, object, created_at, author_email
  ret = db.execute("INSERT INTO tags (name, sha, object, created_at, author_email)
            VALUES (?, ?, ?, ?, ?)", [name, sha, object, created_at, author_email])
end

def parse_file db, file
  commit = nil
  File.open( file ).each_line do |line|
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

db = create_database "commits.db"
db.transaction
parse_file db, "tags.log"
tag_commits db
db.commit
