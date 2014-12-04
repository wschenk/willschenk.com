require 'lib/apiserver'

###
# Blog settings
###

# Time.zone = "EST"

set :markdown, :tables => true, :autolink => true, :gh_blockcode => true, :fenced_code_blocks => true
set :markdown_engine, :redcarpet

set :social, {
  twitter: "http://twitter.com/wschenk",
  tumblr: "http://sublimeguile.com/",
  instagram: "http://instagram.com/wschenk",
  linkedin: "http://www.linkedin.com/pub/will-schenk/0/266/420/",
  github: "https://github.com/wschenk"
}

set :author, "Will Schenk"
set :siteurl, "http://willschenk.com/"
set :disqus_shortname, "willschenk"
set :google_analytics_id, "UA-56296045-1"

activate :autometatags
activate :bootstrap_navbar

activate :blog do |blog|
  # This will add a prefix to all links, template references and source paths
  # blog.prefix = "willschenk.com"

  # blog.permalink = "{year}/{month}/{day}/{title}.html"
  blog.permalink = "{title}.html"
  # Matcher for blog source files
  # blog.sources = "{year}-{month}-{day}-{title}.html"
  # blog.taglink = "tags/{tag}.html"
  # blog.layout = "layout"
  # blog.summary_separator = /(READMORE)/
  # blog.summary_length = 250
  # blog.year_link = "{year}.html"
  # blog.month_link = "{year}/{month}.html"
  # blog.day_link = "{year}/{month}/{day}.html"
  # blog.default_extension = ".markdown"

  blog.tag_template = "tag.html"
  blog.calendar_template = "calendar.html"

  # Enable pagination
  # blog.paginate = true
  # blog.per_page = 10
  # blog.page_link = "page/{num}"
end

activate :directory_indexes
activate :drafts

page "/feed.xml", layout: false
page "/admin/index.html", layout: false

sprockets.append_path File.join root, 'bower_components'
sprockets.append_path File.join root, 'bower_components/mermaid/dist'
sprockets.import_asset 'mermaid.full.min.js'

page "CNAME"

set :css_dir, 'stylesheets'

set :js_dir, 'javascripts'

set :images_dir, 'images'

configure :development do
  activate :livereload
end

# Build-specific configuration
configure :build do
  ignore '/admin/*'
  ignore '/stylesheets/admin/*'

  # For example, change the Compass output style for deployment
  activate :minify_css

  # Minify Javascript on build
  activate :minify_javascript

  # Enable cache buster
  activate :asset_hash

  # Use relative URLs
  activate :relative_assets

  # Or use a different image path
  # set :http_prefix, "/willschenk.com/"
end


activate :deploy do |deploy|
  deploy.method = :git
  deploy.build_before = true
end