class RenameUrlColumn < ActiveRecord::Migration[6.0]
  def self.up
    rename_column :urls, :full_name, :url
  end

  def self.down
    rename_column :urls, :url, :full_name
  end
end
