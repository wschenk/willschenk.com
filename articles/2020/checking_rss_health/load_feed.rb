require 'feedjira'
require 'httparty'

def load_feed feed_url
  resp = HTTParty.get feed_url

  if resp.code == 200
    Feedjira::Feed.add_common_feed_element("generator")

    feed = Feedjira.parse(resp.body)

    info = {
      title: feed.title,
      feed: feed_url,
      link: feed.url,
      description: feed.description,
      generator: feed.generator,
      entries: feed.entries.length
    }

    if feed.entries.length > 0
      info[:last_published] = feed.entries.first.published
      info[:earliest_post] = feed.entries.last.published
    end

    info
  else
    puts "Response status #{resp.code}"
    {}
  end
end

# Test harness
if $PROGRAM_NAME == __FILE__
  feed_info = load_feed "https://willschenk.com/feed.xml"

  feed_info.each { |key,value| puts "|#{key}|#{value}|"}
end
