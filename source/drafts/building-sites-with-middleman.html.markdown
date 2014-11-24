---
title: 'Building Sites with Middleman'
subtitle: 'lean publishing'
tags: middleman, ruby, howto
header_image: books.jpg
---

I make a lot of websites, and I have a certain toolkit that I use to build them.  The most useful things I use are:

- layouts and partials so I only need to set things up once
- `haml` for writing html, since I don't like closing tags
- `Bootstrap` and `sass` for writing css
- `Markdown` for formatting large blogs of content
- `coffeescript` for JavaScript

Middleman is a static site generator, which means that it takes a bunch of source files, does some stuff with it, and produces static HTML, CSS, Images, and Javascript that can be hosted on a basic server somewhere, including [hosting on S3](http://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html) or [Github Pages](https://pages.github.com) so you don't need to consider a server.

Middleman is writting in Ruby, so it's our familiar toolset, and unlike [Jekyll](http://jekyllrb.com) it uses [Sprockets](https://github.com/sstephenson/sprockets), which is the same asset pipelining system that Rails uses.  So you get all the benefits of using a robust system that works for Rails apps without having to learn a bunch more things.

## Setting up a simple static site with Middleman

One of the problems with starting with Middleman is that there are so many places to start.  Lets look at how to setup a basic middleman site with bootstrap-sass, haml, and bower.  First thing is to install middleman:

```sh
$ gem install middleman
```

And create an app:

```sh
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

```rb
gem "middleman-deploy"
gem 'middleman-bootstrap-navbar'
gem "bootstrap-sass"
gem "jquery-middleman"
```

The first two gems expand the functionality of middleman, one to add tasks to push the final site build to `s3`, `gh-pages` and a whole bunch more, and the second to make it easier to build `bootstrap` navbars.

The file two are including `bootstrap-sass` -- the same that we use for rails sites and `jquery`.  These get included into the sprockets asset path, so you don't need to maintain them in your project.  (We'll also see below how to integrate `bower` components into your project.)

## Tweaking Middleman: `config.rb`

This is where we configure how middleman itself works.

```rb
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

`page "CNAME", layout: false`  says to move the file called `CNAME` over without wrapping in the main layout.  Since this file doesn't have .html in the name, it would otherwise get ignored.  This file is for having a custom domain on Github Pages, if that's the sort of thing you are into.

The `set` commands are there to configure different middleman settings, here to show where the various stylesheet, image, and css directories are.

`activate :directory_indexes` with enable various middleman extensions.  Directory indexes means that files named `about.html` will actually get generated into a file called `/about/index.html`, and will rely on the underlying server to have `/about` actually show the directory "index" page, making the urls prettier.  This doesn't work on all servers but works on mostly of them.  This plugin will actually rewrite the output of the `link_to` tags, so you don't need to adjust your templates to work.

` activate :bootstrap_navbar` is an extension that we added in the Gemfile, which makes it easier to generate bootstrap navbars.  Extensions generally work in three ways: they add helper methods, they change the way that the sitemap is processed, or they add different commands to the "middleman" command.

We see an example of that at the bottom of the `config.rb` file, where we configure the `deploy` extension:

```rb
activate :deploy do |deploy|
  deploy.method = :git
  deploy.build_before = true
end
```

This also takes some configuration, and in this case it set to deploy to github pages, and to make sure that it generates the site before doing so.

There are two sections that configure the different middleman environments.  The first is used when you run `middleman server` to look at the site locally:

```rb
configure :development do
  activate :livereload
end
```

The final section is configuring the `build` process, when you run `middleman build` and it creates the generated files in the `build` directory.  The entry below ignores certain files for the build, and runs minifiers over the css and javascript, and turns on _cache busting_.

```rb
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

```rb
sprockets.append_path File.join root, 'bower_components'
```

And then you can put things in your `all.js` file like

```javascript
//= require 'component'
```

To get all of the assets that are included in the packge, you may need to specify the package in the `config.rb` file, such as:

```rb
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

In this usage, this is similar to the way that rails layouts work.

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

## How the build works



_Image Credit: [Moyan Brenn](https://www.flickr.com/photos/aigle_dore/6365101775/in/photolist-aGsP14-7PozJp-84P71r-dphot9-5qz3ks-eqHuv-6NP8d6-HdurS-aibaen-ix8Rbz-6buevW-7acJMF-8DFBf3-MLnGM-dGa2xi-f4HeM7-zXqL6-88og6h-r7w3U-2qKgwi-mcp7h-4eXcGM-9uv3gC-7BAEiV-5RzRtp-7JKRHh-6iUdMZ-2bUDT-8s4PDi-8pvSWc-dynb6k-zV2x5-4xWMzG-dsxGqc-7Cby3b-9t3zWm-b4jsc2-8YyqSQ-9ggca5-aGsPSg-68SVDi-4PNHrt-7JGpz4-aL22G2-q1ic-sZMVW-84NAEC-8JBSSW-9k6cM-89NBaJ)_