# app.rb
require 'sinatra'
require 'sinatra/activerecord'
require 'sqlite3'
require 'json'

set :database, {adapter: "sqlite3", database: "db/development.sqlite3"}

class Trip < ActiveRecord::Base
end

get '/trips' do
  Trip.all.to_json
end

# app.rb (continued)
get '/miles_driven' do
  date = params['date']
  trips = Trip.where("DATE(start_time) = ?", date)
  total_miles = trips.sum("odometer_end - odometer_start")
  { date: date, total_miles: total_miles }.to_json
end

get '/longest_drive' do
  trip = Trip.order("odometer_end - odometer_start DESC").first
  { date: trip.start_time.to_date, miles: trip.odometer_end - trip.odometer_start }.to_json
end

# Implement other endpoints similarly...

# app.rb (continued)
class Charge < ActiveRecord::Base
end

get '/charges' do
  Charge.all.to_json
end

# app.rb (continued)
get '/most_charged_days' do
  charges_by_day = Charge.group("DATE(start_time)").sum(:charge_energy_added_kwh)
  most_charged_days = charges_by_day.sort_by { |_date, kwh| -kwh }.first
  { date: most_charged_days[0], total_kwh: most_charged_days[1] }.to_json
end

# Implement other endpoints similarly...

get '/average_energy_consumption' do
  total_miles = Trip.sum("odometer_end - odometer_start")
  total_energy = Trip.sum(:energy_drawn_kwh)
  average_consumption = total_energy / total_miles
  { average_energy_consumption_kwh_per_mile: average_consumption }.to_json
end

get '/monthly_driving_summary' do
  summary = Trip.group("strftime('%Y-%m', start_time)").select("strftime('%Y-%m', start_time) as month, sum(odometer_end - odometer_start) as total_miles, sum(energy_drawn_kwh) as total_energy").map do |record|
    {
      month: record.month,
      total_miles: record.total_miles,
      total_energy: record.total_energy,
      average_efficiency: record.total_energy / record.total_miles
    }
  end
  summary.to_json
end

get '/trip_efficiency' do
  trips = Trip.all.map do |trip|
    {
      trip_id: trip.id,
      start_time: trip.start_time,
      end_time: trip.end_time,
      miles: trip.odometer_end - trip.odometer_start,
      energy_drawn: trip.energy_drawn_kwh,
      efficiency: trip.energy_drawn_kwh / (trip.odometer_end - trip.odometer_start)
    }
  end
  trips.to_json
end

get '/longest_trips' do
  longest_trips = Trip.order("odometer_end - odometer_start DESC").limit(5).map do |trip|
    {
      trip_id: trip.id,
      start_time: trip.start_time,
      end_time: trip.end_time,
      miles: trip.odometer_end - trip.odometer_start,
      energy_drawn: trip.energy_drawn_kwh
    }
  end
  longest_trips.to_json
end

get '/charging_locations' do
  locations = Charge.select(:location).distinct.map do |charge|
    {
      location: charge.location,
      coordinate: charge.coordinate
    }
  end
  locations.to_json
end

get '/charging_sessions_summary' do
  summary = Charge.group("strftime('%Y-%m-%d', start_time)").select("strftime('%Y-%m-%d', start_time) as day, sum(charge_energy_added_kwh) as total_energy_added, sum(cost_usd) as total_cost").map do |record|
    {
      day: record.day,
      total_energy_added: record.total_energy_added,
      total_cost: record.total_cost
    }
  end
  summary.to_json
end

get '/most_efficient_trips' do
  efficient_trips = Trip.where.not(energy_drawn_kwh: nil).order("(odometer_end - odometer_start) / energy_drawn_kwh DESC").limit(5).map do |trip|
    {
      trip_id: trip.id,
      start_time: trip.start_time,
      end_time: trip.end_time,
      miles: trip.odometer_end - trip.odometer_start,
      energy_drawn: trip.energy_drawn_kwh,
      efficiency: (trip.odometer_end - trip.odometer_start) / trip.energy_drawn_kwh
    }
  end
  efficient_trips.to_json
end

get '/daily_driving_time' do
  time_summary = Trip.group("strftime('%Y-%m-%d', start_time)").select("strftime('%Y-%m-%d', start_time) as day, sum((julianday(end_time) - julianday(start_time)) * 24 * 60) as total_minutes").map do |record|
    {
      day: record.day,
      total_time_minutes: record.total_minutes
    }
  end
  time_summary.to_json
end

get '/common_start_locations' do
  common_starts = Trip.group(:location_start).order('count_id DESC').count(:id).map do |location, count|
    { location: location, trips: count }
  end
  common_starts.to_json
end

get '/common_end_locations' do
  common_ends = Trip.group(:location_end).order('count_id DESC').count(:id).map do |location, count|
    { location: location, trips: count }
  end
  common_ends.to_json
end

get '/trips_by_time_of_day' do
  trips_by_time = Trip.group("strftime('%H', start_time)").order('count_id DESC').count(:id).map do |hour, count|
    { hour: hour, trips: count }
  end
  trips_by_time.to_json
end

get '/average_trip_length' do
  average_length = Trip.average("odometer_end - odometer_start")
  { average_trip_length: average_length }.to_json
end

get '/trip_length_distribution' do
  distribution = Trip.select("odometer_end - odometer_start as length").group(:length).order(:length).count(:id).map do |length, count|
    { length: length, trips: count }
  end
  distribution.to_json
end

get '/frequent_trips' do
  frequent_trips = Trip.select("location_start, location_end, count(*) as trip_count")
                     .group(:location_start, :location_end)
                     .order('trip_count DESC')
                     .limit(10)
                     .map do |trip|
    {
      start_location: trip.location_start,
      end_location: trip.location_end,
      trip_count: trip.trip_count
    }
  end
  frequent_trips.to_json
end

get '/daily_trip_patterns' do
  daily_patterns = Trip.group("strftime('%Y-%m-%d', start_time)").order('count_id DESC').count(:id).map do |date, count|
    { date: date, trips: count }
  end
  daily_patterns.to_json
end

get '/weekly_trip_patterns' do
  weekly_patterns = Trip.group("strftime('%Y-%W', start_time)").order('count_id DESC').count(:id).map do |week, count|
    { week: week, trips: count }
  end
  weekly_patterns.to_json
end

get '/monthly_trip_patterns' do
  monthly_patterns = Trip.group("strftime('%Y-%m', start_time)").order('count_id DESC').count(:id).map do |month, count|
    { month: month, trips: count }
  end
  monthly_patterns.to_json
end

get '/trip_segments' do
  segments = {
    short_trips: Trip.where("odometer_end - odometer_start < ?", 10).count,
    medium_trips: Trip.where("odometer_end - odometer_start BETWEEN ? AND ?", 10, 50).count,
    long_trips: Trip.where("odometer_end - odometer_start > ?", 50).count
  }
  segments.to_json
end
