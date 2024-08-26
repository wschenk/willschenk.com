# db/seeds.rb
require 'csv'

CSV.foreach('path_to_csv_file.csv', headers: true) do |row|
  Trip.create(
    vehicle_name: row['Vehicle Name'],
    vin: row['VIN'],
    timezone: row['Timezone'],
    start_time: "#{row['Start Date']} #{row['Start Time']}",
    end_time: "#{row['End Date']} #{row['End Time']}",
    odometer_start: row['Odometer Start'].to_f,
    odometer_end: row['Odometer End'].to_f,
    energy_drawn_kwh: row['Energy Drawn (kWh)'].to_f,
    charge_energy_added_kwh: row['Charge Energy Added (kWh)'].to_f,
    start_range: row['Start Range ()'].to_f,
    end_range: row['End Range ()'].to_f,
    duration: row['Duration'].to_f,
    location_start: row['Location Start'],
    location_end: row['Location End'],
    coordinate_start: row['Coordinate Start'],
    coordinate_end: row['Coordinate End'],
    cost_usd: row['Cost (USD)'].to_f,
    temperature: row['Temperature ()'].to_f,
    co2: row['CO2 (g)'].to_f,
    gco2_kwh: row['gCO2/kWh'].to_f,
    eq_fuel_burn_gal: row['Eq. Fuel Burn (gal)'].to_f,
    start_soc: row['Start SOC (%)'].to_f,
    end_soc: row['End SOC (%)'].to_f
  )
end

# db/seeds.rb (continued)
CSV.foreach('path_to_csv_file.csv', headers: true) do |row|
  Charge.create(
    vehicle_name: row['Vehicle Name'],
    vin: row['VIN'],
    timezone: row['Timezone'],
    start_time: "#{row['Start Date']} #{row['Start Time']}",
    end_time: "#{row['End Date']} #{row['End Time']}",
    charge_energy_added_kwh: row['Charge Energy Added (kWh)'].to_f,
    start_range: row['Start Range ()'].to_f,
    end_range: row['End Range ()'].to_f,
    connector_type: row['Connector Type'],
    location: row['Location'],
    coordinate: row['Coordinate'],
    cost_usd: row['Cost (USD)'].to_f,
    start_soc: row['Start SOC (%)'].to_f,
    end_soc: row['End SOC (%)'].to_f
  )
end
