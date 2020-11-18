require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'inq'
end

report = Inq.new "rubygems/rubygems", "2019-03-01"

report.save_as "report.html"
