---
title: Personal information from only a URL
subtitle: what can automated tools find out
date: 2014-11-14 00:00 UTC
tags: ruby, socialinvestigator
header_image: core_memory.jpg
---

Ever wonder what you can find out by looking at a url?  How about physical addresses, server location, emails, phone numbers, various links to other profiles (which can in turn be structurally scraped), technology stack, and more. 

```
$ socialinvestigator net page http://willschenk.com/bio
              domain: willschenk.com
          created_on: 2014-10-31
          expires_on: 2015-10-31
          updated_on: 2014-10-31
      registrar_name: ENOM, INC.
       registrar_url: www.enom.com
  registrant_contact: 
                     name: WILL SCHENK
             organization: HAPPYFUNCORP
                  address: 18 BRIDGE STREET, 2E
                     city: BROOKLYN
                      zip: 11201
                    state: NY
             country_code: US
                    phone: +91.76976430
                    email: WSCHENK@GMAIL.COM
         server_name: ec2-54-225-218-167.compute-1.amazonaws.com
      server_country: United States
     server_location: Ashburn, Virginia
     server_latitude: 39.0437
    server_longitude: -77.4875
     server_ip_owner: Amazon Technologies Inc. (AT-88-Z)
              emails: wschenk@gmail.com, will@happyfuncorp.com
               title: Will Schenk
         description: The blog of Will Schenk
      twitter_author: wschenk
         twitter_ids: wschenk
          responsive: true
            rss_feed: /feed.rss
           atom_feed: /feed
       twitter_links: https://twitter.com/wschenk, http://twitter.com/wschenk
      linkedin_links: http://www.linkedin.com/pub/will-schenk/0/266/420/
     instagram_links: http://instagram.com/wschenk
        github_links: https://github.com/wschenk
        technologies: Chartbeat, Font Awesome, Google Analytics, RackCache, Ruby
```

_Standalone code as [a gist](https://gist.github.com/wschenk/7d333acb59b7768f2637), the complete socialinvestigator code [available on github](https://github.com/sublimeguile/socialinvestigator) or and is easily installable on your machine as a gem._

```
$ gem install socialinvestigator
$ socialinvestigator net get_apps_json
$ socialinvestigator net page_info url
```

Poking around different urls can give you a sense of the corporate entities behind sites, who is actually involved, and help you track down people when you can't find them otherwise.  It's actually hard to figure out which URL to include on this post since the data seems so personal and yet people put it out there.  This takes the messy HTML that's out there and returns structured information that you can use to explore profiles on other sites in a way that can be totally automated.

## What does it do

What this code does is to first search for DNS information to see who owns the domain and if there's any contact information associated with it.  It then looks at who owns the IP address and tries to locate where it is geographically.

It then looks at the page itself to see [Open Graph meta data](http://ogp.me),[Twitter Card meta data](https://dev.twitter.com/cards/overview) and other basic SEO tags.

Finally, it looks inside the page for likely looking links to other social networks, and scans the page and HTTP metadata for clues about what underlying technology the site was built in.  (The metadata for the technology fingerprinting is from the [Wappalyzer project](https://github.com/ElbertF/Wappalyzer) which I cobbled together a basic ruby engine for.)

And finally it takes all of the facts that it has collected, figured out which ones take priority, and prints them out.

## Finding Domain info

The first thing that we do is look for the URL and try to find the domain name.  The difference between a hostname and domain name is subtle, partly in some cases they are interchangeable, and partly because DNS is the [second most amazing thing about the Internet](http://codex.happyfuncorp.com/slides/93#1).  (The most truly mind-blowing thing clearly [the default route](http://en.wikipedia.org/wiki/Default_route), the _life, liberty, and pursuit of happiness_ of the Internet.)  A globe spanning, highly distributed database that lets 2.5 billion internet users look up any of the 4 billion potential server addresses in less than 50ms without any real centralized control isn't exactly straightforward.

DNS manages this complexity by delegating authority for different branches of the entire namespace.  The first level is called the Top Level Domains, most famous being _.com_, when you buy a domain name from someone they delegate authority over that name space to you.  These can go deep, especially with large global organizations.  The first thing we do is look for that **Start of Authority** (SOA) record for machine named in the URL.  If we can't find one for that machine, we look up the chain until we find something.

This looks like:

```rb
require 'dnsruby'

hostname = URI(url).hostname

def find_domain( hostname )
  puts "Looking for SOA of #{hostname}"
  dns = Dnsruby::Resolver.new
  soa = dns.query( hostname, "SOA" ).answer.select do |rr|
    rr.is_a? Dnsruby::RR::IN::SOA
  end

  return hostname if soa.length > 0

  # Go from "news.bbc.co.uk" -> "bbc.co.uk"
  parts = hostname.split( /\./ )
  return nil if parts.length <= 2

  find_domain( parts.slice(1,100).join( "." ) )
end
```

Once we've found the domain, we query the `whois` databases to find out who has owns the domain name.  

```rb
require 'whois'

whois = Whois.lookup( domain )

puts "Expires: #{whois.expires_on}"
# Print all contact information we find
whois.contacts.each { |c| puts c }
```

One of the challenges here is that there is no standardized format that there is no standardized way of parsing `whois` responses.  The `whois` gem gives it a serious try:

```
$ ls -l `bundle show whois`/lib/whois/record/parser | wc -l
     209
```

But there's over 500 different whois servers out there, so you won't always get a parseable response.  In that case we print out that we can't find a parser, and we store the unparsed response in the data object as `unparsed_whois`.

```rb
whois.parts.each do |p|
  # Check for responses that we couldn't parse
  if Whois::Record::Parser.parser_for(p).is_a? Whois::Record::Parser::Blank
    puts "Couldn't find a parser for #{p.host}:"
    puts p.body
  end
end
```

## Finding IP and hosting information

Now we look at the IP address, and then do a reverse lookup on it to see what the server machine name is.

```rb
ip_address = Dnsruby::Resolv.getaddress uri.host

data.remember :ip_address, ip_address
begin
  data.remember :server_name, Dnsruby::Resolv.getname( ip_address )
rescue Dnsruby::NXDomain
  # Couldn't do the reverse lookup
end
```

Sometimes interesting things are encoded in the server name, like if it's a Rackspace cloud server vs a Rackspace static server, but we make no attempt to interpret that string.

Then we try and see where the IP address is located geographically, using [freegeoip.net](http://freegeoip.net/).  If you did a lot of this it would make sense to buy a more detailed database from [Maxmind](https://www.maxmind.com/en/home) but for something quick and dirty this works.  Given that you need to follow the rules of the company you are in, it's interesting to see where the servers are located.

```rb
location_info = HTTParty.get('http://freegeoip.net/json/' + ip_address)

data.remember :server_country, location_info['country']
data.remember :server_location, [location_info['city'], location_info['region_name']].select { |x| x }.join( ", ")
data.remember :server_latitude, location_info['latitude']
data.remember :server_longitude, location_info['longitude']
```

We can also do a `whois` lookup on the IP address, to see who owns that IP block.  This should give us an idea of who is hosting the site.  Note that we don't even pretend to parse the `whois` response here in a clever way.

```rb
ip_whois = Whois.lookup ip_address

ip_whois.to_s.each_line.select { |x| x=~/Organization/ }.each do |org|
  if org =~ /Organization:\s*(.*)\n/
    data.another :server_ip_owner, $1
  end
end
```

## Page meta data

Now we load up the page, and look for some basic stuff.  The first thing that we do is load the [meta tags](http://en.wikipedia.org/wiki/Meta_element) into something more accessible.

```rb
response = HTTParty.get url
parsed = Nokogiri.parse response.body

# Meta tags

meta = {}
parsed.css( "meta[name]" ).each do |t|
  meta[t.attributes["name"].value] = t.attributes["content"].value if t.attributes["content"]
end

parsed.css( "meta[property]" ).each do |t|
  meta[t.attributes["property"].value] = t.attributes["content"].value
end
```

Now we load up some basic SEO info, including if there are any feeds for this site's content.

```rb
data.remember( :author, meta['author'] ) 
data.remember( :description, meta['description'] ) 
data.remember( :keywords, meta['keywords'] ) 
data.remember( :generator, meta['generator'])
data.remember( :responsive, true )  if meta["viewport"] =~ /width=device-width/
data.remember( :server, response.headers['server'] )
data.remember( :page_title, parsed.title )

# RSS Feed:
if feed = parsed.css( 'link[type="application/rss+xml"]' ).first
  feed = feed.attributes['href'].value
  data.remember( :rss_feed, feed )
end

# Atom Feed:
if feed = parsed.css( 'link[type="application/atom+xml"]' ).first
  feed = feed.attributes['href'].value
  data.remember( :atom_feed, feed )
end
```

## Twitter Cards

[Twitter Card meta data](https://dev.twitter.com/cards/overview) is a way to control how your data gets displayed on twitter, which has the benefit of defining some summary meta data around the social graph.  One thing thing to note is that `twitter:creator` is the author of this page, while `twitter:site` is the twitter account for the overall site.

```rb
data.remember( :twitter_title, meta["twitter:title"] ) 
data.remember( :twitter_creator, meta["twitter:creator"] ) 
if /@(.*)/.match( meta["twitter:creator"] )
  data.another( :twitter_ids, $1 )
end
data.remember( :twitter_site_author, meta["twitter:site"] )
if /@(.*)/.match( meta["twitter:site"] )
  data.another( :twitter_ids, $1 )
end
data.remember( :twitter_image, meta["twitter:image"] ) 
data.remember( :twitter_description, meta["twitter:description"] )
```

## Open Graph

[Open Graph meta data](http://ogp.me) is really about what your link looks like when someone shares it on Facebook.

```rb
data.remember( :og_title, meta["og:title"] ) 
data.remember( :og_description, meta["og:description"] )
data.remember( :og_type, meta["og:type"] ) 
data.remember( :og_image, meta["og:image"] ) 
```

## Social Page Links

We search for social links:

| Service | Regex |
| ------- | ----- |
| Email | `/mailto:(.*@.*\..*)/` |
| Twitter | `/twitter.com\/[^\/]*$/` |
| LinkedIn | `/linkedin.com/` |
| Instagram | `/instagram.com/` |
| Facebook | `/facebook.com\/[^\/]*$/` |
| Google+ | `/plus.google.com\/[^\/]*$/` |
| Github | `/github.com\/[^\/]*$/` |

For Twitter, Facebook, and Google+ we are only letting through links that have a simple query string, since for the most part this means that it's the user's ID.

## Parsing Twitter Shares and Intents

We then look for Twitter Share links, and try and parse out the user names found in there.

```rb
# Look for twitter shared links

twitter_shared = matching_links( parsed, /twitter.com\/share/ )

twitter_shared.each do |l|
  text = l['data-text']

  # See if there's a "by @user" in the text
  if /by\s*@([^\s]*)/.match text
    data.another( :twitter_ids, $1 )
    data.remember( :twitter_by, $1 ) 
  end

  # Look for all "@usernames" in the text
  if text
    text.split.select { |x| x =~ /@\s*/ }.each do |id|
      data.another( :twitter_ids, id.slice( 1,100 ) ) # We don't want the @
    end
  end

  # See if there's a via link on the anchor tag
  if l['data-via']
    data.another( :twitter_ids, l['data-via'])
  end

  possible_via = URI.decode( (URI(l['href']).query) || "" ).split( /&amp;/ ).collect { |x| x.split( /=/  ) }.select { |x| x[0] == 'via' }
  if possible_via.size > 0
    data.another( :twitter_ids, possible_via[0][1] )
  end
end
```

There are also twitter intent links:

```rb
twitter_intent = hrefs( matching_links( parsed, /twitter.com\/intent/ ) )

twitter_intent.each do |t|
  URI.decode( URI(t.gsub( / /, "+" )).query ).split( /&/ ).select do |x| 
    x =~ /via/
  end.collect do |x| 
    x.gsub( /via=/, "" )
  end.each do |via|
    data.another( :twitter_ids, via )
  end
end
```

## Technology Finger Prints

The final thing we do is to load the `apps.json` file from [Wappalyzer](https://github.com/ElbertF/Wappalyzer) which is a cross-platform utility that uncovers the technologies used on websites.  This has a list of regex for the header tags, meta tags, scripts and other parts of the html to make guesses about which technology is in place.  What is in place is very rudimentary, but it gives a general sense of what is used to made the site.

## Installation

The standalone code as [a gist](https://gist.github.com/wschenk/7d333acb59b7768f2637), and you can check out the complete [socialinvestigator code on github](https://github.com/sublimeguile/socialinvestigator).  To run this on your machine:

```
$ gem install socialinvestigator
$ socialinvestigator net get_apps_json
$ socialinvestigator net page_info http://willschenk.com/bio
```

It may take a while to get the responses.  If you want to see everything it's doing, use the `--debug` switch

```
$ socialinvestigator net page_info http://willschenk.com/bio --debug
```

The reverse lookup can take a while, and if you want to turn that off:

```
$ socialinvestigator net page_info http://willschenk.com/bio --noreverse
```
