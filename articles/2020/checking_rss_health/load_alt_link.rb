require 'nokogiri'
require 'httparty'

def find_feed_from_url feed_url
  puts "Looking at #{feed_url}"
  resp = HTTParty.get feed_url, timeout: 10

  if resp.code == 200
    page = Nokogiri.parse resp.body

    page.css( 'head link[rel="alternate"]' ).each do |x|
      url = x['href']
      return URI.join( feed_url, x['href'] ).to_s
    end
  else
    puts "Unable to load #{feed_url} #{resp.code}"
  end

  nil
end

# Test harness
if $PROGRAM_NAME == __FILE__
  puts find_feed_from_url( "http://willschenk.com" )
  puts find_feed_from_url( "http://tomcritchlow.com/" )
end
