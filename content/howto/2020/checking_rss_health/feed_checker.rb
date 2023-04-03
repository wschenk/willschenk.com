require 'csv'
require './load_dns'
require './load_feed'
require './load_opml'
require './load_alt_link'

headers = [ :site_url, :feed_url, :old_feed_url, :resolved,
            :registered, :created_on, :updated_on, :title, :link,
            :description, :generator, :entries, :earliest_post,
            :last_published]


CSV.open "feed_info.csv", "w" do |csv|
  csv << headers

  feeds = load_opml "feeds.opml"
  feeds.each do |feed|
    whois = {}
    feed_info = {}

    site_url = feed[:site_url]
    feed_url = feed[:feed_url]
    feed[:old_feed_url] = feed_url

    if !site_url.nil? && site_url != ""
      # Look for updated feed
      begin
        puts "Check to see if #{feed_url} has an update"

        new_feed_url = find_feed_from_url site_url
        if new_feed_url
          feed_url = new_feed_url
          feed_info[:feed_url] = feed_url
        end
      rescue 
        puts "Error looking up #{site_url}"
      end
    end

    begin
      puts "Getting whois for #{feed_url}"
      whois = load_dns feed_url
      if whois[:resolved]
        puts "Loading #{feed_url}"
        feed_info = load_feed feed_url
        pp feed_info
      else
        puts "#{feed_url} doesn't resolve"
      end
    rescue Resolv::ResolvError
      puts "No host"
    rescue Exception => e
      puts "Error with #{feed_url}"
      puts e
      puts "************"
    end

    csv << headers.collect { |x| feed[x] || whois[x] || feed_info[x] }
  end
end
