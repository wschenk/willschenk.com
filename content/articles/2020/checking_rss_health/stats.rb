require 'csv'

stats = { total: 0, resolved: 0, last_month: 0, last_year: 0,
          has_entries: 0, total_entries: 0, generator: {} }

headers = nil

CSV.open( "feed_info.csv" ).each do |line|
  if headers.nil?
    headers = line
  else
    stats[:total] += 1
    if line[headers.index( "resolved" )]
      stats[:resolved] += 1
    end

    entries = line[headers.index( "entries" )]
    if !entries.nil?
      stats[:has_entries] += 1
      stats[:total_entries] += entries.to_i
    end

    updated_on = line[headers.index( "last_published" )]
    if !updated_on.nil? && updated_on != ""
      updated_on = Date.parse( updated_on )
      days_old = (Time.now.to_date - updated_on).to_i

      if days_old < 30
        stats[:last_month] += 1
      end

      if days_old < 365
        stats[:last_year] += 1

        g = line[headers.index( "generator" )]
        # Collapse some where the username is in the generator field
        g = 'LiveJournal' if g =~ /LiveJournal/
        g = 'Tumblr' if g =~ /Tumblr/
        stats[:generator][g] ||= 0
        stats[:generator][g] += 1
      end
    end
  end
end

puts "| Total | #{stats[:total]} |"
puts "| Resolved | #{stats[:resolved]} |"
puts "| Active with 30 days | #{stats[:last_month]} |"
puts "| Active within a year | #{stats[:last_year]} |"
puts "| Feeds with entries | #{stats[:has_entries]} |"
puts "| Total posts | #{stats[:total_entries]} |"

g = stats[:generator]
g.keys.sort { |a,b| g[b] <=> g[a]}.each do |generator|
  puts "| #{generator} | #{g[generator]} |" if g[generator]>1
end
