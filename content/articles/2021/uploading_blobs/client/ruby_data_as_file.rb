require 'net/http'

def write_string_blob( host, data )
  uri = URI(host)
  req = Net::HTTP::Post.new( uri.path )
  req.set_form([['file', 'This is my string', {filename: 'test'}]], 'multipart/form-data')

  res = Net::HTTP.start(uri.hostname, uri.port) do |http|
    http.request(req)
  end

  res.body
end

puts write_string_blob( 'http://localhost:8080/put', 'this is my data' )
