---
title: 'Migration Generators'
# date: TBD When publishing
tags:
---

module HappySeed
  module Generators
    class OmniauthGenerator < Rails::Generators::Base
      include Rails::Generators::Migration
      source_root File.expand_path('../templates', __FILE__)

      def install_omniauth
        # if File.exists? 'app/models/identity.rb'
        #   puts "identity.rb already exists, skipping"
        #   return
        # end

        migration_template("make_email_nullable.rb", "db/migrate/make_email_nullable.rb" )

