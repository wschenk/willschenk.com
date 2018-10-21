---
title: Bower with Rails
subtitle: Javascript dependancies and sprockets
date: 2015-07-25
tags:
  - javascript
  - rails
header_image: train_hat.jpg
obsolete: true
aliases:
  - "/bower-with-rails/"
---



Let's look at how we can integrate bower with rails and Sprockets. [Bower](http://bower.io) is one of the two major javascript package managers, the other being the Node Package Manager, or [npm](https://www.npmjs.com).  The biggest difference between the two is how the javascript is packaged up.  Bower is the more straightforward of the two, and it mainly a way to distribute javascript and keep things up to date.

## Install Bower
The first step is to install bower.  Ironically, we need to use npm to do that!  On OSX, the easiest way is to use the [Homebrew](http://brew.sh)

```bash
$ brew install node
```

Then let's install bower in your local command path so you can run it.

```bash
$ npm install -g bower
```

## Setup Bower

Now let's setup rails to work with bower.  This section assumes that you already have a rails application.

```bash
cd rails_application
```

Create a `.bowerrc` file. This is going to tell bower where to install the packages it needs, and we're going to say put it in `vendor/assets/bower_components`:

```json
{
  "directory": "vendor/assets/bower_components"
}
```

Now run `bower init` to create a `bower.json` file.  This file is similar to a `Gemfile`, in that it lists out the dependancies, and when you run bower again, it will download the specific versions from the intertubes.

## Tell rails where to look

Inside of `config/application.rb` we're going to tell spockets and additional directory to look for javascript components.

```ruby
config.assets.paths << Rails.root.join('vendor', 'assets', 'bower_components')
```

## Small walk through

Let's say you want to use Google Maps with Angular.  [angular-google-maps](http://angular-ui.github.io/angular-google-maps/#!/) is a package that does a lot of the heavy lifting for you, so lets install that.  We're going to tell bower to keep track of the packages we want inside of our `bower.json` file by passing in the `--save` option:

```
$ bower install --save angular-google-maps
[...]
angular-google-maps#2.1.5 vendor/assets/bower_components/angular-google-maps
├── angular#1.4.3
└── lodash#3.10.0

lodash#3.10.0 vendor/assets/bower_components/lodash

angular#1.4.3 vendor/assets/bower_components/angular
```

I've cleaned up some of the output, but you can see that bower installed angular-google-maps and it's dependancies, angular itself and lodash.  Lets go to our `application.js` file now to tell Sprockets that we want to use this awesome stuff:

```js
//= require lodash
//= require angular
//= require angular-google-maps
```

Since we've added `vendor/assets/bower_components` in Sprockets load path, it should be able to find all of these files.  Notice that you don't need to know where to get all of the dependancies from, but you do need to make sure that you specify them in your `application.js` in the right order.  We'll be talking about `browersify` in future posts that assuages this issue.

## To check in or not to check in

I prefer to check in the dependancies inside of my source control.  This isn't necessary and in some ways is a waste of disk space, but I think it's easier to figure out what's going on if there's less magic.  This can be confusion if you start to make local tweaks of the javascript, which you should never do.

However, it's not necessary to check things in if you expand your build process a bit.  If you look inside of your `bower.json` file, you'll see that it specifies the specific version of the bower package that you installed.

```js
{
  "name": "rails_app",
  "version": "0.0.0",
  "authors": [
    "Will Schenk <wschenk@gmail.com>"
  ],
  "license": "MIT",
  "ignore": [
    "**/.*",
    "node_modules",
    "bower_components",
    "test",
    "tests"
  ],
  "dependencies": {
    "angular-google-maps": "~2.1.5"
  }
}
```

This should contain all the information needed so that when you run `bower install` again, all of the correct versions of the files will be downloaded.  You could make it part of your build process, much like how running `bundle install` is part of the standard rails build pack.  Bundler looks through `Gemfile.lock` and installs those specific packages.  The version is always specified inside of `bower.json` so there doesn't need to be two files.
