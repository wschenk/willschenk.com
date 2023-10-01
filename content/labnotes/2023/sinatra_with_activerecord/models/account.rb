# models/account.rb
class Account < ActiveRecord::Base
  validates :name, uniqueness: true, presence: true

  has_secure_password
end
