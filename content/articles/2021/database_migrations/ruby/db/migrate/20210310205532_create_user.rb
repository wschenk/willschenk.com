class CreateUser < ActiveRecord::Migration[6.0]
  def self.up
    create_table :urls do |t|
      t.string :full_name
      t.datetime :added
      t.boolean :active, default: true
    end
  end

  def self.down
    drop_table :urls
  end
end
