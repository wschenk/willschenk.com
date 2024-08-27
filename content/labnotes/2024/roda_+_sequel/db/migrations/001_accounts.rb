# db/migrations/001_accounts.rb

puts "Loading up migration"

Sequel.migration do
  change do
    create_table(:accounts) do
      primary_key :id
      String :email, null: false
      Boolean :confirmed, default: false
      String :name
      String :login_hash
      DateTime :hash_valid_until, null: true
      DateTime :created_at, null: false
      DateTime :updated_at, null: false
    end
  end
end
