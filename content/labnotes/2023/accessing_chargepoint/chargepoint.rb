#!/usr/bin/env ruby

require 'httparty'

uri = 'https://sso.chargepoint.com/api/v1/user/login'
query = {
  "username": "wschenk@gmail.com",
  "timezone_offset": 240,
  "timezone": "America/New_York"
}

page = HTTParty.post uri, query: query
p page.headers
p page.body
