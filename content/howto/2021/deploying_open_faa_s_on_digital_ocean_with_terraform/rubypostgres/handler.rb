require 'json'
require 'database_url'
require 'pg'

class Handler
  def run(req)
    c = DatabaseUrl.to_active_record_hash(File.read( '/var/openfaas/secrets/host' ) )

#    {"adapter":"postgresql","host":"private-gratitude-postgres-cluster-do-user-1078430-0.b.db.ondigitalocean.com","database":"defaultdb","port":25060,"user":"doadmin","password":"ievezzbyz0a1stxa"}

    # Output a table of current connections to the DB
    conn = PG.connect(
      c[:host],
      c[:port],
      nil,
      nil,
      c[:database],
      c[:user],
      c[:password] )

    r = []
    conn.exec( "SELECT * FROM pg_stat_activity" ) do |result|
      r << "     PID | User             | Query"
      result.each do |row|
        r << " %7d | %-16s | %s " %
             row.values_at('pid', 'usename', 'query')
      end
    end

    return r.join( "\n" );
  end
end
