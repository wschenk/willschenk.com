# Begin Admin Tool

require 'sprockets/coffee-react'
require 'lib/apiserver.rb'

set :server, 'thin'

map "/api" do
  run ApiServer
end

page "/admin/*html", layout: false

activate :react

::Sprockets.register_preprocessor 'application/javascript', ::Sprockets::CoffeeReact
::Sprockets.register_engine '.cjsx', ::Sprockets::CoffeeReactScript
::Sprockets.register_engine '.js.cjsx', ::Sprockets::CoffeeReactScript

after_configuration do
  sprockets.append_path File.dirname(::React::Source.bundled_path_for('react.js'))
end

set :debug_assets, true

# End Admin Tool

# For custom domains on github pages
page "CNAME", layout: false

set :css_dir, 'stylesheets'
set :js_dir, 'javascripts'
set :images_dir, 'images'

# Better markdown support
set :markdown, :tables => true, :autolink => true, :gh_blockcode => true, :fenced_code_blocks => true, with_toc_data: true, disable_indented_code_blocks: true
set :markdown_engine, :redcarpet

activate :autometatags

sprockets.append_path File.join root, 'webicons'
# sprockets.append_path File.join root, 'bower_components'
# sprockets.import_asset 'mermaid'

# Automatic image dimensions on image_tag helper
# activate :automatic_image_sizes

# Easier bootstrap navbars
activate :bootstrap_navbar
activate :graphviz

activate :blog do |blog|
  # This will add a prefix to all links, template references and source paths
  # blog.prefix = "blog"

  # blog.permalink = "{year}/{month}/{day}/{title}.html"
  blog.permalink = "{title}.html"

  # Matcher for blog source files
  # blog.sources = "{year}-{month}-{day}-{title}.html"
  # blog.taglink = "tags/{tag}.html"
  blog.layout = "article_layout"
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

# Turn this on if you want to make your url's prettier, without the .html
activate :directory_indexes

page "/feed.xml", layout: false
activate :drafts

configure :development do
  activate :livereload do |lr|
    lr.ignore = ['/admin/*']
  end
end

# Build-specific configuration
configure :build do
  # Any files you want to ignore:
  ignore '/admin/*'
  ignore '/stylesheets/admin/*'
  ignore '/javascripts/admin/*'

  # For example, change the Compass output style for deployment
  activate :minify_css

  # Minify Javascript on build
  activate :minify_javascript

  # Enable cache buster
  activate :asset_hash

  # Use relative URLs
  activate :relative_assets

  # Or use a different image path
  # set :http_prefix, "/blog/"
end


# This will push to the gh-pages branch of the repo, which will
# host it on github pages (If this is a github repository)
activate :deploy do |deploy|
  deploy.method = :git
  #deploy.build_before = true
end

helpers do
  def chapters( post )
    File.readlines( post.source_file ).collect do |x|
      if x =~ /^##\s(.*)/
        $1
      else
        nil
      end
    end.select { |x| x }
  end


  def tag_nav
    prev_post = {}
    next_post = {}

    article_list = blog.articles
    me = article_list.index current_article
    if me
      next_post[:chronological] = article_list[me-1] if me != 0
      prev_post[:chronological] = article_list[me+1] if me+1 != article_list.length
      current_article.tags.each do |t| 
        article_list = blog.tags[t]
        me = article_list.index current_article
        next_post[t] = article_list[me-1] if me != 0
        prev_post[t] = article_list[me+1] if me+1 != article_list.length
      end
      prev_spots = {}
      next_spots = {}
      prev_post.each do |k,v|
        prev_spots[v] ||= []
        prev_spots[v] << k.capitalize
      end
      next_post.each do |k,v|
        next_spots[v] ||= []
        next_spots[v] << k.capitalize
      end

      { prev: prev_spots, next: next_spots }
    else
      { prev: [], next: [] }
    end
  end
end
