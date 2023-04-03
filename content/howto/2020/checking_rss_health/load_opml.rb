require 'nokogiri'

def load_opml feed_file
  doc = Nokogiri.XML(File.read(feed_file))

  doc.xpath( "//body//outline" ).select do |x|
    x['xmlUrl']
  end.collect do |x|
    { title: x['title'], feed_url: x['xmlUrl'], site_url: x['htmlUrl'] }
  end
end

if $PROGRAM_NAME == __FILE__
  load_opml( "feeds.opml" ).each do |x|
    printf "%35s %s %s\n", x[:title], x[:feed_url], x[:site_url] if x[:feed_url]
  end
end
