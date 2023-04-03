require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'faraday'
  gem 'json'
end

def package_info name
  response = Faraday.get "https://registry.npmjs.org/#{name}"

  if response.status == 200
    data = JSON.parse response.body

    data
  else
    :unknown
  end
end

pp package_info( "@yarnpkg/lockfile" )
