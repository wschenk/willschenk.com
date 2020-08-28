require 'sqlite3'

# Query the database and pivot the second result (filename) around the
# first result (either date or commit id) into a cochange structure
def query_and_loop
  db = SQLite3::Database.new "test.db"

  cochanges = yield db

  file_set = []
  prev_id = nil

  ret = {}

  cochanges.each do |id,name|
    if prev_id != id
      add_set ret, file_set
      file_set = []
    end
    file_set << name unless file_set.include? name
    prev_id = id
  end
  add_set ret, file_set
  collect_and_sort ret
end

# Add each set into the cochange structure.  This tracks how many
# times each file was seen with another file, and the number of times
# the file was seen overall.
def add_set cochange, set
  set.each do |file|
    cochange[file] ||= {}
    cochange[file][:count] ||= 0
    cochange[file][:count] += 1
    set.each do |cofile|
      if file != cofile
        cochange[file][cofile] ||= 0
        cochange[file][cofile] += 1
      end
    end
  end
end

# Turn the cochange sent into an array that is sorted by the
# cooralation
def collect_and_sort cochange
  ret = []

  cochange.each do |file, stats|
    commits = stats[:count]
    stats.each do |cofile, cocount|
      coorelation = cocount / commits.to_f
      if cofile != :count && coorelation > 0.3 && cocount > 2
        ret << { file: file,
                 commits: commits,
                 cofile: cofile,
                 coorelation: coorelation,
                 cocount: cocount }
      end
    end
  end

  ret.sort { |a,b| b[:coorelation] <=> a[:coorelation] }
end

# Prints the results into a table that org-mode can easily reformat.
def print_cochange cochange
  puts "| file | commits | cofile | coorelation | count |"
  puts "|-|-|-|-|-|"

  cochange.each do |file|
    puts "|#{file[:file]}|#{file[:commits]}|#{file[:cofile]}|#{(file[:coorelation]*100).to_i}%|#{file[:cocount]}|"
  end
end
