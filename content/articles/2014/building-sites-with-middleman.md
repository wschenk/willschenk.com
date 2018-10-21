---
title: Building Sites with Middleman
subtitle: lean publishing
tags:
  - middleman
  - ruby
  - howto
  - tools
header_image: books.jpg
date: 2014-11-25
obsolete: true
aliases:
  - "/building-sites-with-middleman/"
---
I make a lot of websites, and I have a certain toolkit that I use to build them.  The most useful things I use are:

- layouts and partials so I only need to set things up once
- `haml` for writing html, since I don't like closing tags
- `Bootstrap` and `sass` for writing css
- `Markdown` for formatting large blogs of content
- `coffeescript` for JavaScript

Middleman is a static site generator, which means that it takes a bunch of source files, does some stuff with it, and produces static HTML, CSS, Images, and Javascript that can be hosted on a basic server somewhere, including [hosting on S3](http://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html) or [Github Pages](https://pages.github.com) so you don't need to consider a server.

Middleman is written in Ruby, so it's our familiar toolset, and unlike [Jekyll](http://jekyllrb.com) it uses [Sprockets](https://github.com/sstephenson/sprockets), which is the same asset pipelining system that Rails uses.  So you get all the benefits of using a robust system that works for Rails apps without having to learn a bunch more things.

In the node world [Yeoman](http://yeoman.io) does something similar, but I personally have had poor luck getting [Grunt](http://gruntjs.com) to work reliably in practice.

## Setting up a simple static site with Middleman

One of the problems with starting with Middleman is that there are so many places to start.  Lets look at how to setup a basic middleman site with bootstrap-sass, haml, and bower.  First thing is to install middleman:

```bash
$ gem install middleman
```

And create an app:

```bash
$ middleman new static_site
[...]
      create  static_site/.gitignore
      create  static_site/config.rb
      create  static_site/source/index.html.erb
      create  static_site/source/layouts/layout.erb
      create  static_site/source/stylesheets
      create  static_site/source/stylesheets/all.css
      create  static_site/source/stylesheets/normalize.css
      create  static_site/source/javascripts
      create  static_site/source/javascripts/all.js
      create  static_site/source/images
      create  static_site/source/images/background.png
      create  static_site/source/images/middleman.png
```

This creates 4 files in the main directory, `Gemfile` and `Gemfile.lock`, which we know and love, `config.rb` which configures how middleman generates the site, and `source` which are the sourcefiles of the site.

## Gemfile

By default, `middleman` installs the `middleman-livereload` plugin, so in development mode any browers that have a page open with refresh when you save a file.  This makes testing a lot easier.  We can install other gems here to add different functionality.  Let's add a few of these now:

```ruby
gem "middleman-deploy"
gem 'middleman-bootstrap-navbar'
gem "bootstrap-sass"
gem "jquery-middleman"
```

The first two gems expand the functionality of middleman, one to add tasks to push the final site build to `s3`, `gh-pages` and a whole bunch more, and the second to make it easier to build `bootstrap` navbars.

The file two are including `bootstrap-sass` -- the same that we use for rails sites -- and `jquery`.  These get included into the sprockets asset path, so you don't need to maintain them in your project.  (We'll also see below how to integrate `bower` components into your project.)

## Tweaking Middleman

`config.rb` is where we configure how middleman itself works.

```ruby
# For custom domains on github pages
page "CNAME", layout: false

set :css_dir, 'stylesheets'
set :js_dir, 'javascripts'
set :images_dir, 'images'

# Better markdown support
# set :markdown, :tables => true, :autolink => true, :gh_blockcode => true, :fenced_code_blocks => true
# set :markdown_engine, :redcarpet

# Turn this on if you want to make your url's prettier, without the .html
activate :directory_indexes

# Automatic image dimensions on image_tag helper
# activate :automatic_image_sizes

# Easier bootstrap navbars
activate :bootstrap_navbar

configure :development do
  activate :livereload
end

# Build-specific configuration
configure :build do
  # Any files you want to ignore:
  ignore '/admin/*'

  # For example, change the Compass output style for deployment
  activate :minify_css

  # Minify Javascript on build
  activate :minify_javascript

  # Enable cache buster
  activate :asset_hash

  # Use relative URLs
  activate :relative_assets
end


# This will push to the gh-pages branch of the repo, which will
# host it on github pages (If this is a github repository)
activate :deploy do |deploy|
  deploy.method = :git
  deploy.build_before = true
end
```

Lets go through this in detail.

`page "CNAME", layout: false`  says to move the file called `CNAME` over without wrapping in the main layout.  Since this file doesn't have .html in the name, it would otherwise get ignored.  This file is for having a custom domain on Github Pages, if that's the sort of thing you are in to.

The `set` commands are there to configure different middleman settings, here to show where the various stylesheet, image, and css directories are.

`activate :directory_indexes` with enable _pretty urls_.  Directory indexes means that files named `about.html` will actually get generated into a file called `/about/index.html`, and will rely on the underlying server to have `/about` actually show the directory "index" page, making the urls prettier.  This doesn't work on all servers but works on most of them.  This plugin will actually rewrite the output of the `link_to` tags, so you don't need to adjust your templates to work.

`activate :bootstrap_navbar` is an extension that we added in the Gemfile, which makes it easier to generate bootstrap navbars.  Extensions generally work in three ways: they add helper methods, they change the way that the sitemap is processed, or they add different commands to the "middleman" command.

We see an example of that at the bottom of the `config.rb` file, where we configure the `deploy` extension:

```ruby
activate :deploy do |deploy|
  deploy.method = :git
  deploy.build_before = true
end
```

This also takes some configuration, and in this case it set to deploy to github pages, and to make sure that it generates the site before doing so.  Here is the [middleman-deploy](https://github.com/karlfreeman/middleman-deploy) github page with documentation.

There are two sections that each configure a different middleman environment.  The first is used when you run `middleman server` to look at the site locally:

```ruby
configure :development do
  activate :livereload
end
```

The final section is configuring the `build` process, when you run `middleman build` and it creates the generated files in the `build` directory.  The entry below ignores certain files for the build, and runs minifiers over the css and javascript, and turns on _cache busting_.

```ruby
# Build-specific configuration
configure :build do
  # Any files you want to ignore:
  ignore '/admin/*'

  # For example, change the Compass output style for deployment
  activate :minify_css

  # Minify Javascript on build
  activate :minify_javascript

  # Enable cache buster
  activate :asset_hash

  # Use relative URLs
  activate :relative_assets
end
```

## Using bower

The basic way to use bower is to put this in `config.rb`:

```ruby
sprockets.append_path File.join root, 'bower_components'
```

And then you can put things in your `all.js` file like

```javascript
//= require 'component'
```

To get all of the assets that are included in the packge, you may need to specify the package in the `config.rb` file, such as:

```ruby
sprockets.import_asset 'jquery'
```

More information on the [Middleman Asset Pipline documentation](http://middlemanapp.com/basics/asset-pipeline/)

## Using layouts and partials

The `source` directory is where the actual code for your site lives.  Here is an example `layouts/layout.haml` file to give you a sense of how to use layouts and include partials:

```haml
!!! 5
%html.no-js.sticky
  %head
    %meta{ :charset => 'utf-8' }/
    %meta{ 'http-equiv' => 'X-UA-Compatible', :content => 'IE=edge,chrome=1' }/

    %title This site is amazing!

    %meta{ :name => 'description', :content => '' }/
    %meta{ :name => 'viewport', :content => 'width=device-width' }/
    = stylesheet_link_tag 'application', "socicons", "animate"

  %body
    = partial "layouts/main_header"

    ~ yield

    = partial "layouts/footer"
    = partial "layouts/javascripts"
```

In this usage, this is similar to the way that rails layouts work.  I'm using the `~` HAML operator inside of `=` here because I want to make sure that it doesn't do anything wonky with the indention on `<pre>` blocks, btw.

Middleman has a concept of nested layouts, which lets you have wrap an a layout around another one.  I think that this is confusing in practice, but as an example you could have a `layouts/sidebar_layout.erb` that looked like:

```erb
<% wrap_layout :layout do %>
	<div class="sidebar">
		<%= partial "layouts/sidebar" %>
	</div>

	<div class="content">
		<%= yield %>
	</div>
<% end %>
```

## The YAML preable the preyaml
Metadata about the template is included in a block of text at the top of the file, which gets pulled off to set things that the templating system can use later.  On the top of this file that I am editing right now, it looks like this

```
---
title: 'Building Sites with Middleman'
subtitle: 'lean publishing'
tags: middleman, ruby, howto
header_image: books.jpg
---

I make a lot of websites, and I have a certain toolkit that I use to build...
```

The `title`, `subtitle`, `tags` and `header_image` attributues are available in the templates as page data, so you can access them like:

```haml
  %h1= current_article.title
  %h2= current_article.data['subtitle']
```
_title_ is built into middleman, _subtitle_ and _header image_ are just some random things I made up.  The `tags` attribute is part of the `middleman-blog` extension which we will cover below.

## How the build works

When you run `middleman server` or `middleman build`, middleman loads up the configuration file in `config.rb`.  It creates a sitemap based upon the files in the `source` directory as well as other directives inside of the config.rb file.

By default it only includes files like `.html.erb` and `.js`, but you can set it manually include a non-template file (like our `page "CNAME"` above or create other `proxy` files.  Proxy files are a way of seperating out the templates from the source data.

```ruby
# Assumes the file source/about/template.html.erb exists
["tom", "dick", "harry"].each do |name|
  proxy "/about/#{name}.html", "/about/template.html", :locals => { :person_name => name }
end
```
This creates three entries into the sitemap called `/about/tom.html`, `/about/dick.html` and `/about/harry.html` that use a specific template.

This data doesn't need to be hardcoded into the `config.rb` btw, you can also place `json` and `yml` files in the `data/` directory which middleman will load automatically.  For example, `data/employees.json`

```json
[{"name":"Tom","title":"Janitor"},{"name":"Mary","title":"CEO"}]
```
And then in your `config.rb` you could access this as:

```ruby
data['employees'].each do |employee|
  proxy "/about/#{name}.html", "/about/template.html", :locals => { :person_name => employee[:name], :title => employee[:title] }
end
```
Then each entry in the sitemap the file is processed (based upon the extension, so scss -> js, haml -> html, etc.) into the build directory.  Helper methods are available inside of the templates for things like `javascript_include_tag`, `stylesheet_link_tag`, `link_to` and `image_tag` and all of the rest.

## middleman server

Starting the server in preview mode will start a local server on port `4567` that generates the files on demand.  If you have livereload enabled this will automatically trigger a page refresh for any open browsers, so you can tweak and look at things as you go.

Inside of your templates, `config.environment == :development` when you are in preview mode.  So, if there are some things that you don't want to push to the live site but are useful for development, you can switch them on and off using that mechanism.

## middleman build

This does basically the same thing as the server, but the templates are generally further processed.  Cachebusting can be enabled, and you can include tracking code if `config.environment == :build` is true.  This goes through all of the files in source that look like webfiles and places them in the `build` directory.

And easy way to check out what you have there is by `cd`ing into the `build/` directory and running a simple webserver to serve the pages.  Relative links don't work when you open the file directly in the browser, so you need to use an actual webserver.

```bash
$ cd build
$ python -m SimpleHTTPServer
```
And then open a new browser on port `8000`.

## middleman deploy

In the basic `:git` setting that we have above, middleman deploy will build the site into the `build/` directory, switch that directory to the `gh-pages` branch, and push it to `origin`.  Assuming that you are hosting your repo on github, this will publish the static content on github pages.

If you want to use a [custom domain](https://help.github.com/articles/setting-up-a-custom-domain-with-github-pages/) then you need to create a `CNAME` file in `source/` with the domain name, and set up your DNS records to match.

One thing to note is that while changes to the pages seem to deploy quickly, it takes a long time for the first push to github pages to show up, on the order of 10-15 minutes.

More information on [middleman-deploy](https://github.com/karlfreeman/middleman-deploy).

## Building a blog

There are two good extensions for building a blog with middleman.  The default template for blog is sort of confusing in the way that it's laid out, mainly because it gets rid of the `layouts/` directory, but let's go through it and see how it's supposed to work:

```bash
$ gem install middleman-blog
$ middleman new static_blog --template=blog
      create  static_blog/.gitignore
      create  static_blog/config.rb
      create  static_blog/source
      create  static_blog/source/2012-01-01-example-article.html.markdown
      create  static_blog/source/calendar.html.erb
      create  static_blog/source/feed.xml.builder
      create  static_blog/source/index.html.erb
      create  static_blog/source/layout.erb
      create  static_blog/source/tag.html.erb
      create  static_blog/source/stylesheets
      create  static_blog/source/javascripts
      create  static_blog/source/images
```

Now we have another middleman site, with a bunch of files.  It also creates a new command:

```bash
$ middleman article "This is the title of my article"
```
Let's also include the `middleman-blog-drafts` gem into the `Gemfile`, `activate :drafts` in `config.rb, and that will give us a few more commands:

```bash
$ middleman draft "This is amazing"
      create  source/drafts/this-is-amazing.html.markdown
$ middleman publish source/drafts/this-is-amazing.html.markdown
      create  source/2014-11-25-this-is-amazing.html.markdown
      remove  source/drafts/this-is-amazing.html.markdown
```
This lets us keep drafts in git and doesn't force us to commit to a date until we are ready to publish it.  There are `published` and `date` attributes that the default blog extension knows about to turn it on and off, and is a good example of something that you can see in development but not production, but that still leaves to moving files around manually to adjust the date.

## What does it add

In addition to the sitemap, we now have a `blog`, `article`, and `tag` concept inside of the middleman app.  Articles are represented as pages (by default using `markdown`) but the `tag` and `calendar` templates are actually more like `proxy` templates than file templates, and when the site is generated middleman will iterate over then to produce many output files from one template.

Part of the `index.html.haml` of this site looks like:

```haml
  - (drafts + page_articles).each do |article|
    .post
      .post-date
        - unless article.is_a? ::Middleman::Blog::Drafts::DraftArticle
          %p= article.date.strftime( '%b %e' )
        - else
          %p.draft Draft
```
I'm putting all of the articles in a list, both drafts and published ones, and only showing the date for articles which have already been published.

Here's what gets added to the `config.rb`:

```ruby
activate :blog do |blog|
  # This will add a prefix to all links, template references and source paths
  # blog.prefix = "blog"

  # blog.permalink = "{year}/{month}/{day}/{title}.html"
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

activate :drafts
```

This should be enough to get you started, and more [documentation is here](http://middlemanapp.com/basics/blogging/) .  At this point it really becomes a design and development challenge, not figuring out how to use the tool.

## Go get started

Middleman gives you all of the front-end developer benefits of using a system like Rails, but outputs static content than can be served anywhere without any dependancies.  Many sites don't really require all that for them to run, and it's crazy to me that something as read heavy as a blog often can't perform well under load given that it's just serving up the same old stuff over and over again.  You want to have some tooling to make it easier, but it doesn't need to be _run time_ tooling.

_Image Credit: [Moyan Brenn](https://www.flickr.com/photos/aigle_dore/6365101775/in/photolist-aGsP14-7PozJp-84P71r-dphot9-5qz3ks-eqHuv-6NP8d6-HdurS-aibaen-ix8Rbz-6buevW-7acJMF-8DFBf3-MLnGM-dGa2xi-f4HeM7-zXqL6-88og6h-r7w3U-2qKgwi-mcp7h-4eXcGM-9uv3gC-7BAEiV-5RzRtp-7JKRHh-6iUdMZ-2bUDT-8s4PDi-8pvSWc-dynb6k-zV2x5-4xWMzG-dsxGqc-7Cby3b-9t3zWm-b4jsc2-8YyqSQ-9ggca5-aGsPSg-68SVDi-4PNHrt-7JGpz4-aL22G2-q1ic-sZMVW-84NAEC-8JBSSW-9k6cM-89NBaJ)_
