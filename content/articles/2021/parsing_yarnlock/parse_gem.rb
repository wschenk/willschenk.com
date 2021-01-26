require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'yarnlock'
end

data = File.read( '/home/wschenk/gratitude/yarn.lock' )

parsed = Yarnlock.parse data

pp parsed
