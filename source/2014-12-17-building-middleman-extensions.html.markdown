---
:title: Building Middleman Extensions
:subtitle: make middleman more awesome
:tags: middleman, howto, ruby
:date: 2014-12-17
---
Middleman extensions, like rails plugins, are packaged as gems.  There are three main ways to extend middleman.  You can add helpers, add middleman commands, or extend the sitemap generation in someway.  Lets go through those in detail.

## Creating the extension

Create a gem using `bundle gem _name_`

```sh
$ bundle gem middleman-graphviz
```

Add `middleman-core` to your gem dependancies in the `.gemspec` file:

```rb
  spec.add_runtime_dependency     'middleman-core', ['>= 3.0.0']
```

Register your extension into middleman.  Our gem will be activated in the sites `config.rb` using `activate :graphviz` and this is how middleman knows what to load.  `lib/middleman/graphviz.rb`:

```rb
require 'middleman-core'
require "middleman/graphviz/version"
require "middleman/graphviz/extension"

::Middleman::Extensions.register(:graphviz, ::Middleman::Graphviz::Extension )
```

_Replace with your gem name!_

Write the code that actually plugs into middleman.  The we are going to add some helpers to the site, so let's register them here.  Create `lib/middleman/graphviz/extension.rb`

```rb
require 'middleman/graphviz/helpers'

module Middleman
  module Graphviz
    class Extension < Middleman::Extension
      def initialize( app, options_hash = {}, &block)
        super

        app.helpers Middleman::Graphviz::Helpers
      end
    end
  end
end
```

_Replace with your gem name!_

## Writing helpers

Lets create a basic helper method now in the file `lib/middleman/graphviz/helpers.rb`. This is going to accept one parameter and a block. We are going to get the content of that block and then spit it back directly for now.

```rb
module Middleman
  module Graphviz
    module Helpers
      def basic_helper_example( param )
        "<h1>#{param}</h1>".html_safe
      end

      def block_helper_example( type, &block )
        if block_given?
          data = capture_html(&block)

          data = data.upcase

          concat_content(data.html_safe)
        end
      end
    end
  end
end
```

These can be used like

```erb
<%%= basic_helper_example( "My Title") %>
```

or as a block

```erb
<%% block_helper( "My Title") do %>
  This is going to be in upcase
<%% end %%>
```

## Add it to an active middleman project

Have bundler reference this new gem inside of an existing middleman project.  `Gemfile`:

```rb
gem "middleman-graphviz", path: "../middleman-graphviz"
```

Inside of the middleman project's `config.rb` activate it:

```rb
activate :graphviz
```

Now startup the middleman server, and use your helper in the page!  Note that, just like when you use a helper defined in `config.rb` you need to restart `middleman server` to see your change take effect.

## Adding configuration to your extension

If you want to include configurable options in your extension, here's some skeleton code for `extension.rb`:

```rb
require 'middleman/graphviz/helpers'

module Middleman
  module Graphviz
    cattr_accessor :options
    
    class Extension < Middleman::Extension
      def initialize( app, options_hash = {}, &block)
        super

        app.helpers Middleman::Graphviz::Helpers
      end

      def registered(app, options_hash = {}, &block)
        options = Options.new(options_hash)
        yield options if block_given?

        # Default options for the rsync method.
        options.theme ||= "default"
        options.generate_file ||= false

        ::Middleman::Graphviz.options = options

        app.helpers Middleman::Graphviz::Helpers
      end

      def after_configuration
        puts "After configuration"
      end
    end

    module Helpers
      def options
        ::Middleman::Graphviz.options
      end
    end
  end
end
```

## Adding additional middleman commands

The middleman command is built on [thor](/making-a-command-line-utility-with-gems-and-thor), which as we know is awesome.  To add a command to the middleman, use the following template for each `command.rb`:

```rb
require 'middleman-core/cli'

module Middleman
  module Cli
    # This class provides a "deploy" command for the middleman CLI.
    class Graphviz < Thor
      include Thor::Actions

      check_unknown_options!

      namespace :graphviz

      # Tell Thor to exit with a nonzero exit code on failure
      def self.exit_on_failure?
        true
      end

      desc 'graphviz [options]', 'Run this amazing command'
      method_option 'clean',
        type: :boolean,
        aliases: '-c',
        desc: 'Clean all the build files'
      def graphviz
        @shared_instance = ::Middleman::Application.server.inst

        puts "Running my command"
      end
    end
  end
end
```

Then, inside of `middleman/graphviz.rb` simply `require 'middleman/graphviz/command'`.

The [`Thor::Actions`](http://www.rubydoc.info/github/wycats/thor/Thor/Actions) class gives you access to many different helper commands that make it easy to move, filter, template, and otherwise mangle files, and of course you have access to the full middleman app.

## Manipulating the sitemap

The most awesomest way to extend middleman is by modifying the sitemap.  This lets us create whole new URLs that are derived and generated from other sources.  This is how the [middleman-blog](https://github.com/middleman/middleman-blog) extension really works, and if you want to start an indepth exploration that's a good place to start spelunking.

Lets build an extesion that creates pages from an external datasource.  In this case, a CSV file, but you could imagine having this come out of a database instead.  This lets us manipulate the sitemap of the site it's been created, and will let us add and remove different pages based upon what middleman knows about the site.

First we add an `after_configuration` handler to `Middleman::Graphviz::Extension` to register our class as a `resource_list_manipulator`:

```rb
  def after_configuration
    @csv_pages = Middleman::CSV::Page.new( @app, self )
    @app.sitemap.register_resource_list_manipulator(:"csv_pages", @csv_pages, false)
  end
```

Then we create `lib/middleman/csv/page.rb`.

```rb
require 'csv'

module Middleman
  module CSV
    class Page
      # include UriTemplates

      def initialize(app, controller)
        @sitemap = app.sitemap
        @page_link_template = "pages/{page}.html"
        @page_template = "page.html"
        app.ignore @page_template
      end

      # Update the main sitemap resource list
      # @return [void]
      def manipulate_resource_list(resources)
        resources + ::CSV.open( "pages.csv" ).collect do |row|
          create_page_resource( row[0], row )
        end
      end

      private

      def link(page)
        ::Middleman::Util.normalize_path @page_link_template.gsub( /\{page\}/, page )
      end

      def create_page_resource( page, row )
        Sitemap::Resource.new(@sitemap, link(page)).tap do |p|
          p.proxy_to( @page_template )

          p.add_metadata locals: {
            row: row
          }
        end
      end
    end
  end
end
```

When `initialized` is called, we are grabbing references to the middleman app.  We've hard coded two things in this example that should be from the options, the page name template, and the page template itself.  Since the `page.html` isn't meant to be standalone, we tell middleman to ignore it in the sitemap with `app.ignore @page_template`.

When `manipulate_resource_list` is called it is passed a list of resources that middleman currently knows about.  We return a new list of resources based on that list.  What we are doing in this example is loading up the list of pages from a file called `pages.csv`, creating new pages based on that, and then returning the new list.

This example is contrived and we are loading in the list of pages from the filesystem.  At this point in the rendering process we do have access to all of the pages, instead of loading up a file you could inspect the site map, go through all of the pages and make additional pages for subsets of those.  The `middleman-blog` does this for both `tag` pages and `calendar` pages.  Instead of calling `::CSV.open` it goes through all of the pages on the site and collects a dynamic list of pages based upon the metatag, either `tag` or published dates.

Then we create the entries in the sitemap themselves. These first get proxied to our page template.  Then we set the metadata of the particular page to be what we loaded in from the file.

Here's an example template `page.html.haml`:

```haml
%h1= row[1]
= row[0] 
```

This example is equivelent to looping over a file in `config.rb` and setting up page proxies there.  However in that case we don't have access to the sitemap overall, so we couldn't generate a dynamic list of new pages based upon existing pages.  With this extension we can insert ourselves into the rendering process and add the _awesome_.

## Conclusion

Building these extensions is a very complicated way to achive things that would be simplier if you just build the site in rails and had a database with you at runtime.  But it works, and you can achieve plug into the rendering process to create a more _dynamic_ static site.  Helpers are by far the easiest way to package things together, and as you can see from the example I'm working through I'm working on an easy way to integrate Graphviz images in middleman documents without a seperate workflow.  (_Coming soon._)

Middleman CLI commands are also easy to build, though here you do start to need to know more about how the internals of how middleman is setup.  Things like the `middleman-deploy` gem are pretty amazing and can really help with the overall publishing workflow.

And sitemap manipulation is the most powerful, which lets you recreate pages based upon site and page metadata that is collected throughout the process.  The entire `middleman-blog` extention is build using this functionality.

Happy building!
