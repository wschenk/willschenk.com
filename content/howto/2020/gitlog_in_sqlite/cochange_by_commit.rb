require './query_and_pivot.rb'

cochanges = query_and_loop do |db|
  db.execute "
    select id, name from commit_files where id in 
      (select commits.id from commits, commit_files 
       where commits.id = commit_files.id and author_when >= ?
      )", ['2011-01-01']
end

print_cochange cochanges
