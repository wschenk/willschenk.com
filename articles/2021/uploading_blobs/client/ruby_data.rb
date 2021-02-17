require 'net/http'

res = Net::HTTP.post( URI( 'http://localhost:8080/put' ), 'This is my string' )

puts res.body
