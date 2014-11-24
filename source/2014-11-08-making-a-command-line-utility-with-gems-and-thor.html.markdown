---
title: Making a command line utility with gems and thor
subtitle: Any excuse to the use the phrase "Hammer of the Gods"
date: 2014-11-08 00:00 UTC
tags: ruby, thor, socialinvestigator, howto
header_image: thor.jpg
---

Some scripts work well because they are self contained and don't have a lot of dependancies, like the [hosts on your network tracker](http://willschenk.com/how-to-track-your-coworkers).

Others scripts
- have more code than fits into a single file
- multiple options and switches
- have an extensive set of dependancies

And on those cases, its better to make a gem and use thor

## Hammer of the Gods

Lets figure out how to make some command line tools and package them up so that they can be shared and used by other people.  Being able to write a script for one off tasks or simple automation is often a lot easier than building out a full app.  There are a lot of libraries and gems out there that make it easy to get information from out there on _the web_, and they generally require a little glue to make it work.

This article is a walk through in building out a command line utility that will let you pass in a URL that will search [hacker news](https://news.ycombinator.com) for any mentions.  In further postings, we'll see how to integrate twitter and google analytics searching.

## First, building a beautiful gem

[Rubygems](http://rubygems.org) is the standard package manage for ruby, and [Bundler](http://bundler.io) is the best way to manage dependancies for your application.  `Bundler` is what makes you `Gemfile`s work.  

If you don't have the `bundler` gem installed, you probably don't have [rvm installed.](http://rvm.io).  You should go ahead and do that.

`bundle gem` is a command that will generate a template for building a gem.  It will create a standard directory structure, create a git repo, and make it easy to build out gems, install them locally, and push them up to [the central gem repository](http://rubygems.org/).

```bash
$ bundle gem socialinvestigator
      create  socialinvestigator/Gemfile
      create  socialinvestigator/Rakefile
      create  socialinvestigator/LICENSE.txt
      create  socialinvestigator/README.md
      create  socialinvestigator/.gitignore
      create  socialinvestigator/socialinvestigator.gemspec
      create  socialinvestigator/lib/socialinvestigator.rb
      create  socialinvestigator/lib/socialinvestigator/version.rb
Initializing git repo in /Users/wschenk/src/socialinvestigator
```

The first file to look at is the `socialinvestigator.gemspec`.  This defines information about your gem, a description, homepage url, the files that are included, and all of it's dependancies.  There are two types of dependencies:

1. **Runtime dependencies** are what the gem needs to be installed and functional when its running.
2. **Development dependencies** are additional gems needed for building the gem, which normally mean gems needed for testing and building.

Lets edit the file and add a line

```rb
spec.add_dependency 'thor'
spec.add_dependency 'httparty'
```

Giving us something like this:

```rb
# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'socialinvestigator/version'

Gem::Specification.new do |spec|
  spec.name          = "socialinvestigator"
  spec.version       = Socialinvestigator::VERSION
  spec.authors       = ["Will Schenk"]
  spec.email         = ["wschenk@gmail.com"]
  spec.summary       = %q{Simple command line tool to look at urls.}
  spec.description   = %q{Simple command line tool to look at urls.}
  spec.homepage      = ""
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0")
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_dependency 'thor'
  spec.add_dependency 'httparty'

  spec.add_development_dependency "bundler", "~> 1.6"
  spec.add_development_dependency "rake"
end
```

And run `bundle` to install.

## Creating a binary

Now we need to create a binary in the `bin` folder, which doesn't yet exist.  The line `spec.executables` in the `.gemspec` is what tells rubygems which files are being included and installed with the gem.  The definition that the bundler gem template gives you will include all files in the `bin` directory which are checked into `git`.

Lets create `bin/socialinvestigator` now:

```rb
#!/usr/bin/env ruby -wU

require 'socialinvestigator'

puts "Hello, world!"
```

Now we need to make it executable,

```sh
wschenk$ chmod +x bin/socialinvestigator
```

## Running the binary

Running this binary now by typing `bin/socialinvestigator` will give an error.  This is because it's being called in your normal user context.  The first line in the file does `require 'socialinvestigator'`, which is the _gem we are writing_, and that _gem hasn't been installed_.

We can run this in the context of the gem itself, by running

```sh
$ bundle exec bin/socialinvestigator 
Hello, world!
```

You should always run your gem this way.  The `bundle exec` command will load all of the gems specified in the `Gemfile.lock` and then start your script in that context.  This is also going to be important later on, when you are working on a new version while you have an older version installed.

---

## Thor

[Thor](http://whatisthor.com) is a toolkit for building command-line interfaces.  The `bundle` command itself is implemented in `thor`.  Thor makes it easy to expose methods in your class, with parameters and options, to the command line.  Let's see how it works.

We are going to add a new file in `lib/socialinvestigator` called `cli.rb`.

```rb
require 'thor'

module Socialinvestigator
  class HammerOfTheGods < Thor
    desc "hello NAME", "This will greet you"
    long_desc <<-HELLO_WORLD

    `hello NAME` will print out a message to the person of your choosing.

    Brian Kernighan actually wrote the first "Hello, World!" program 
    as part of the documentation for the BCPL programming language 
    developed by Martin Richards. BCPL was used while C was being 
    developed at Bell Labs a few years before the publication of 
    Kernighan and Ritchie's C book in 1972.

    http://stackoverflow.com/a/12785204
    HELLO_WORLD
    option :upcase
    def hello( name )
      greeting = "Hello, #{name}"
      greeting.upcase! if options[:upcase]
      puts greeting
    end
  end
end
```
 
_The art of naming variables, classes and methods is one that I've honed over years of progressional software engineering, based largely on both my experience as an inheritor of other's inexplicable code, as well as the practical jokes that I, evidently, liked to play on my future self.  Also, I was inspired by [The Bonhamizer](http://static.echonest.com/bonhamizer/go.html?trid=TRSBVUT12F87DF0212)_

Lets make sure that we require that new file in the main `lib/socialinvestigator.rb` file:

```rb
require "socialinvestigator/version"
require "socialinvestigator/cli"

module Socialinvestigator
  # Your code goes here...
end
```

And now lets change our `bin/socialinvestigator` ruby scripts to:

```rb
#!/usr/bin/env ruby -U

require 'socialinvestigator'

Socialinvestigator::HammerOfTheGods.start( ARGV )
```

This creates a class/ _bon mot_ named `Socialinvestigator::HammerOfTheGods` that we can now place our code in.  We've changed our script to call the class method `Socialinvestigator::HammerOfTheGods.start( ARGV )`, which passes in the command like arguments into the Thor base class.  These arguments are parsed, and Thor looks for public method on our class, with the right number of arguments, to run when passed on the command line.

Running it with no arguments will print out a list of all the commands available.  In our case, only the build in `help` command, and our `hello` command:

```sh
$ bundle exec bin/socialinvestigator 
Commands:
  socialinvestigator hello NAME      # This will greet you
  socialinvestigator help [COMMAND]  # Describe available commands or one specific command
```

Lets try running our command with the wrong number of arguments, _i.e. none_.  Here it will print out the short usage of the command that we specified with the `desc` DSL.

```sh
$ bundle exec bin/socialinvestigator hello
ERROR: "socialinvestigator hello" was called with no arguments
Usage: "socialinvestigator hello NAME"
```

The built in help command will bring out usage information for the method using the `long_desc` if available and the regular description if not.  These are optional but why not, right?  Notice also how it's smart enough to figure out the command name, in this case `socialinvestigator`

```sh
$ bundle exec bin/socialinvestigator help hello
Usage:
  socialinvestigator hello NAME

Options:
  [--upcase=UPCASE]  

Description:
  `hello NAME` will print out a message to the person of your choosing.

  Brian Kernighan actually wrote the first "Hello, World!" program as part 
  of the documentation for the BCPL programming language developed 
  by Martin Richards. BCPL was used while C was being developed at 
  Bell Labs a few years before the publication of Kernighan and Ritchie's 
  C book in 1972.

  http://stackoverflow.com/a/12785204
```

Lets now run the command as it was meant to be:

```sh
$ bundle exec bin/socialinvestigator hello world
Hello, world
```

And when passing in an optional tag:

```sh
$ bundle exec bin/socialinvestigator hello world --upcase
HELLO, WORLD
```

## Multi-Thor

You can also mount Thor classes inside of other ones.  This is handy because generally you want a few top level functions that do broad sweeping things, and then many more very specific method that do fiddly things with an API that you don't often use.  

This is done with the `subcommand` method.  Inside of `lib/socialinvestigator/cli.rb` lets add the lines in the `HammerOfTheGods` class:

```rb
require 'thor'
require 'socialinvestigator/cli/hn'

module Socialinvestigator
  class HammerOfTheGods < Thor
    [...]

    desc "hn COMMANDS", "Hacker News Control Module"
    subcommand "hn", Socialinvestigator::CLI::Hn
  end
end
```

And now lets create that new file `lib/socialinvestigator/cli/hn.rb`:

```rb
module Socialinvestigator
  module CLI
    class Hn < Thor
      desc "search URL", "Search hn.algolia.com for a url mentioned on Hackernews"
      option :tags
      def search( url )
        puts "Looks like you are looking for #{url} with tags #{options[:tags]}"
      end
    end
  end
end
```

And we can now see what we have:

```sh
$ bundle exec bin/socialinvestigator 
Commands:
  socialinvestigator hello NAME      # This will greet you
  socialinvestigator help [COMMAND]  # Describe available commands or one specific command
  socialinvestigator hn COMMANDS     # Hacker News Control Module

$ bundle exec bin/socialinvestigator help hn
Commands:
  socialinvestigator hn help [COMMAND]  # Describe subcommands or one specific subcommand
  socialinvestigator hn search URL      # Search hn.algolia.com for a url mentioned on Hackernews

$ bundle exec bin/socialinvestigator hn search http://google.com --tags post
Looks like you are looking for http://google.com with tags post
```

---

## API Interlude

I wrote a bunch more code and then *[checked it in to github](https://github.com/sublimeguile/socialinvestigator)*.

---

## Getting the gem out there: Build, install, release

In order to install the gem, we need to build it:

```sh
$ rake build
```

This will create a gem in the `pkg` directory.  The version, in our case, is specified in `lib/socialinvestigator/version.rb` and will need to bump it up everytime we push a release out.

Lets install the gem locally, and see if we can access what we need ourside of the working directory:

```sh
$ rake install
```

This takes the gem located in `pkg` and installs it as part of our local gem set.  Now we can type anywhere on our system:

```sh
$ socialinvestigator hn search willschenk.com/how-to-track-your-coworkers
1 Hits
How to track your coworkers – Simple passive network surveillance
  http://willschenk.com/how-to-track-your-coworkers
  40 points
  18 comments
  https://news.ycombinator.com/item?id=8541102
```

Now lets release it.

```
$ rake release
socialinvestigator 0.0.1 built to pkg/socialinvestigator-0.0.1.gem.
Tagged v0.0.1.
Pushed git commits and tags.
Pushed socialinvestigator 0.0.1 to rubygems.org.
```

This command tags the repo, pushes the commits and tags onto github, and then pushes the code to rubygems.org, where it gets its [own shiny page](http://rubygems.org/gems/socialinvestigator).

_Image Credit: [JD Hancock](https://www.flickr.com/photos/jdhancock/4756872724/in/photolist-7E5DeZ-8fmdwY)_