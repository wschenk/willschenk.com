# db/migrate/001_create_trips.rb
class CreateTrips < ActiveRecord::Migration[6.0]
  def change
    create_table :trips do |t|
      t.string :vehicle_name
      t.string :vin
      t.string :timezone
      t.datetime :start_time
      t.datetime :end_time
      t.float :odometer_start
      t.float :odometer_end
      t.float :energy_drawn_kwh
      t.float :charge_energy_added_kwh
      t.float :start_range
      t.float :end_range
      t.float :duration
      t.string :location_start
      t.string :location_end
      t.string :coordinate_start
      t.string :coordinate_end
      t.float :cost_usd
      t.float :temperature
      t.float :co2
      t.float :gco2_kwh
      t.float :eq_fuel_burn_gal
      t.float :start_soc
      t.float :end_soc
    end
  end
end
