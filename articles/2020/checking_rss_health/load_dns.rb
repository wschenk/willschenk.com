require 'resolv'
require 'domainator'
require 'whois'
require 'whois-parser'

def load_dns feed_url
  uri = URI.parse feed_url

  info = {}
  info[:hostname] = uri.hostname

  # See if the name is still on the internet
  info[:resolved] = Resolv.getaddress uri.hostname

  # Find the domain from the url
  info[:domain] = Domainator.parse(feed_url)

  # Lookup whois to see how old it is
  client = Whois::Client.new
  record = client.lookup(info[:domain])
  result = record.parser

  info[:registered] = result.registered?
  info[:created_on] = result.created_on
  info[:updated_on] = result.updated_on
  info[:expires_on] = result.expires_on

  info
end

if $PROGRAM_NAME == __FILE__
  pp load_dns( "https://codex.happyfuncorp.com/feed" )
  pp load_dns( "https://aestheticsofjoy.com/feed" )
end
