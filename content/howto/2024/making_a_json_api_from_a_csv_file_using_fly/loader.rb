require 'csv'
require 'sqlite3'

class Loader
  def initialize
    @dir = ENV['DB_DIR'] || '.'
  end

  def db; "#{@dir}/db"; end
  def csv; "#{@dir}/csv"; end
  
  def db_exists?; File.exists? db; end
  def csv_exists?; File.exists? csv; end

  def ensure!
    if !db_exists?
      if !csv_exists?
        puts "Downloading csv"
        download_csv
      end
    end

    if !db_exists?
      create_db
    end
  end
  
  def download_csv
    puts "Downloading csv"
    system( "curl https://willschenk.com/alt_fuel_stations.csv -o #{csv}" )
  end

  def create_db
    puts "Creating database"

    system( "sqlite-utils insert #{db} data #{csv} --csv --detect-types" )
  end
end

if __FILE__ == $0
  puts "Hello there"

  l = Loader.new
  puts "DB Exists? #{l.db_exists?}"
  puts "CSV Exists? #{l.csv_exists?}"

  l.ensure!

  puts "DB Exists? #{l.db_exists?}"
  puts "CSV Exists? #{l.csv_exists?}"
end
