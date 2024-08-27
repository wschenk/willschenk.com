# frozen_string_literal: true
require_relative '../db'
require 'securerandom'

class Account < Sequel::Model
  def generate_login_hash
    self.login_hash = SecureRandom.hex(16)
    self.hash_valid_until = Time.now + 3600 # 1 hour in seconds
    save
  end
end
