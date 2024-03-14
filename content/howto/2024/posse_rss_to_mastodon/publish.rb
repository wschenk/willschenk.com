require 'bundler/inline'
require 'net/http'

# Required gems
gemfile do
  source 'https://rubygems.org'
  gem "yaml", "~> 0.3.0"
  gem "feedjira", "~> 3.2"
  gem "tty-prompt", "~> 0.23.1"
  gem "thor"
end

def config
  config_file = 'config.yml'

  begin
    return YAML.safe_load(File.read(config_file), symbolize_names: true)
  rescue StandardError => e
    puts "Error loading configuration: #{e.message}"
    exit 1
  end
end

def download(url)
  uri = URI( url )

  response = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) do |http|
    req = Net::HTTP::Get.new(uri)
    
    http.request(req)
  end

  response
end

def mastodon_posts( account )
  uri = URI("#{account}.rss")

  response = download(uri)

  feed = Feedjira.parse(response.body)
end

def print_feed( feed )
  feed.entries.each do |entry|
    # Print the entry title and URL
    puts "Title: #{entry.title}"
    puts "URL: #{entry.url}"
    puts "Date: #{entry.published}"
    p entry
    exit
    puts entry.summary
    puts "-------------------------"
  end
end

def links_from_feed(feed)
  links = []
  feed.entries.each do |entry|
    URI.extract( entry.summary ) do |uri|
      links << uri
    end
  end

  links.sort.uniq
end

def blog_feed( feed )
  uri = URI(feed)
  
  response = download(uri)
  
  feed = Feedjira.parse(response.body)
end

def blog_posts( feed )
  blog_entries(feed).entries
end

def toot_links
  feed = mastodon_posts( "#{config[:server]}/@#{config[:user]}" )
  links_from_feed feed
end

def feed_links
  feed = blog_feed( config[:feed] )
  feed.entries.collect{ |entry| entry.url }
end

def show_diffs
  puts "Getting toot_links"
  tl = toot_links

  puts "Getting feed_links"
  fl = feed_links

  puts "Shared links"
  (tl & fl).each do |l|
    puts l
  end
  puts

end

def post_to_mastodon(server, token, message)
  uri = URI("#{server}/api/v1/statuses")
  req = Net::HTTP::Post.new(uri)
  req["Authorization"] = "Bearer #{token}"
  req.set_form_data("status" => message)

  response = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) do |http|
    http.request(req)
  end

  if response.is_a?(Net::HTTPSuccess)
    puts "Successfully posted message to Mastodon."
  else
    puts "Error posting to Mastodon: #{response.message}"
  end
end

def syndicate( random = false )
  tl = toot_links
  feed = blog_feed( config[:feed] )

  feed.entries.shuffle! if random

  prompt = TTY::Prompt.new

  feed.entries.each do |entry|
    link = entry.url
    if tl.index link
      puts "#{link} already posted"
    else
      puts "#{link} not posted"
      fmt = "%10s %s\n"
      printf fmt, "Title", entry.title
      printf fmt, "Date", entry.published

      if prompt.yes?( "Post?" )
        summary = prompt.ask("Post text:")

        if prompt.yes?( "Confirm post?" )
          message = "#{summary} #{link}"
          puts "Posting #{message}"
          post_to_mastodon( config[:server], config[:token], message )
        end
      end
    end
  end
end

class MyCLI < Thor
  desc "took MESSAGE", "post a message"
  def toot message
    post_to_mastodon( config[:server], config[:token], message )
  end

  desc "toots", "show a users toots"
  def toots
    feed = mastodon_posts( "#{config[:server]}/@#{config[:user]}" )
    print_feed feed
  end

  desc "links_of_toots", "show a list of things that the user linked to"
  def links_of_toots
    feed = mastodon_posts( "#{config[:server]}/@#{config[:user]}" )
    require 'pp'
    pp links_from_feed( feed )
  end

  desc "feed_urls", "show a list of posts"
  def feed_urls
    feed = blog_feed( config[:feed] )
    feed.entries.each do |entry|
      puts entry.url
    end
  end

  desc "diffs", "show the difference in links"
  def diffs
    show_diffs
  end

  desc "sync", "put it together"
  option :random, type: :boolean, default: false
  def sync
    syndicate( options[:random] )
  end
end

MyCLI.start(ARGV)
