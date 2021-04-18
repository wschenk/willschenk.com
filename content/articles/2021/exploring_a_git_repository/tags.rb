require 'sqlite3'

def create_database filename
  db = SQLite3::Database.new filename

  rows = db.execute <<-SQL1
       CREATE TABLE IF NOT EXISTS tags (
              name TEXT UNIQUE,
              sha TEXT,
              object TEXT,
              created_at DATETIME
              );
SQL1

  return db
end

def add_tag db, name, sha, object, created_at
  ret = db.execute("INSERT INTO tags (name, sha, object, created_at)
            VALUES (?, ?, ?, ?)", [name, sha, object, created_at])
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

      begin
        add_tag db, tag_name, sha, sha_object, created_at
      rescue SQLite3::ConstraintException
        puts "Found existing tag, exiting"
        exit
      end
    end
  end
end

db = create_database "commits.db"
db.transaction
parse_file db, "tags.log"
db.commit
