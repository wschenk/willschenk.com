require 'csv'
require 'date'

csv = []

authors_day_time = {}
authors_week_time = {}

File.readlines( ARGV[0] || "commits.log" ).each do |line|
  date_string, email, author = line.split( "|" )

  date = DateTime.parse( date_string )

  day_time = nil

  case date.hour
  when 0..4
    day_time = "late night"
  when 5..6
    day_time = "dawn"
  when 6...8
    day_time = "early morning"
  when 8..12
    day_time = "mid morning"
  when 12..18
    day_time = "afternoon"
  when 18..22
    day_time = "evening"
  when 22..24
    day_time = "late night"
  end

  authors_day_time[author] ||= {}
  authors_day_time[author][:total] ||= 0
  authors_day_time[author][:total] += 1
  authors_day_time[author][day_time] ||= 0
  authors_day_time[author][day_time] += 1

  week_time = nil

  case date.wday
  when 1..5
    week_time = "weekday"
  else
    week_time = "weekend"
  end
   
  authors_week_time[author] ||= {}
  authors_week_time[author][:total] ||= 0
  authors_week_time[author][:total] += 1
  authors_week_time[author][week_time] ||= 0
  authors_week_time[author][week_time] += 1
end

CSV.open( ARGV[1] || "worktime.csv", "w" ) do |csv|
  csv << ['author', 'total', 'dawn', 'early_morning', 'mid_morning', 'afternoon', 'evening', 'late_night', 'weekday', 'weekend']

  authors_day_time.keys.sort do |a,b|
    authors_day_time[b][:total] <=> authors_day_time[a][:total]
  end.each do |author|
    ret = [author, authors_week_time[author][:total]]

    ['dawn', 'early morning', 'mid morning', 'afternoon', 'evening', 'late night'].each do |day_time|
      ret << (authors_day_time[author][day_time] || 0).to_f / authors_day_time[author][:total].to_f
    end

    ret << (authors_week_time[author]['weekday'] || 0).to_f / authors_week_time[author][:total].to_f
    ret << (authors_week_time[author]['weekend'] || 0).to_f / authors_week_time[author][:total].to_f

    csv << ret
  end
end

  
