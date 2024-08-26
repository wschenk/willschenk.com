# db/migrate/002_create_charges.rb
class CreateCharges < ActiveRecord::Migration[6.0]
  def change
    create_table :charges do |t|
      t.string :vehicle_name
      t.string :vin
      t.string :timezone
      t.datetime :start_time
      t.datetime :end_time
      t.float :charge_energy_added_kwh
      t.float :start_range
      t.float :end_range
      t.string :connector_type
      t.string :location
      t.string :coordinate
      t.float :cost_usd
      t.float :start_soc
      t.float :end_soc
    end
  end
end
