require 'csv'

headers = nil

CSV.open( "feed_info.csv" ).each do |line|
  if headers.nil?
    headers = line
  else
    updated_on = line[headers.index( "last_published" )]
    if !updated_on.nil? && updated_on != ""
      updated_on = Date.parse( updated_on )
      days_old = (Time.now.to_date - updated_on).to_i

      if days_old < 365
        puts "** #{line[headers.index( "feed_url" )]}"
      end
    end
  end
end
