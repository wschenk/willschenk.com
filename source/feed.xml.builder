xml.instruct!
xml.feed "xmlns" => "http://www.w3.org/2005/Atom" do
  site_url = data['site'].siteurl
  xml.title data['site'].title
  xml.subtitle "Lets build things"
  xml.id URI.join(site_url, blog.options.prefix.to_s)
  xml.link "href" => URI.join(site_url, blog.options.prefix.to_s)
  xml.link "href" => URI.join(site_url, current_page.path), "rel" => "self"
  xml.updated(Time.now.iso8601) unless blog.articles.empty?
  xml.author { xml.name data['site'].author }

  blog.articles[0..5].each do |article|
    xml.entry do
      xml.title article.title
      xml.link "rel" => "alternate", "href" => URI.join(site_url, article.url)
      xml.id URI.join(site_url, article.url)
      xml.published article.date.to_time.iso8601
      if article.data['update']
        xml.updated article.data['update'].to_time.iso8601
      else
        xml.updated article.date.to_time.iso8601
      end
      xml.author { xml.name data['site'].author }
      # xml.summary article.summary, "type" => "html"
      xml.content article.body, "type" => "html"
    end
  end
end
