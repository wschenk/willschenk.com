require 'net/http'

def write_file_blob( host, file )
    uri = URI(host)
    req = Net::HTTP::Post.new( uri.path )
    req.set_form([['file', File.open( file )]], 'multipart/form-data')

    res = Net::HTTP.start(uri.hostname, uri.port) do |http|
      http.request(req)
    end

    res.body
end

puts write_file_blob( 'http://localhost:8080/put', '/home/wschenk/mobiledownloads/talk.pdf' )
