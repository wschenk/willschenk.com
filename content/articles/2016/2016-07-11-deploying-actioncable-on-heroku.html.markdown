---
title: Deploying ActionCable on Heroku with Sidekiq
subtitle: you're gonna need a bigger redis
date: "2016-07-11"
tags: howto, rails, heroku, sidekiq
header_image: cables.jpg
---

ActionCable is WebSockets on rails.  This lets you create realtime, interactive systems, where you can push data from one client to another client without reloading or polling.  But how do we deploy it on heroku?

ActionCable is composed to two main parts: a javascript client library, and a backend pub/sub system built upon Redis.  We're also going to use ActiveJob to offload the publishing tasks from the main user thread, so we'll also be setting up sidekiq.

Let's go through the steps of how to deploy things on heroku.

## First, the app

I'm going to use an app that [dhh made as a walk through.](https://medium.com/@dhh/rails-5-action-cable-demo-8bba4ccfc55e#.q92qfr3c2)  What it does is

- creates a message model
- creates a rooms controller with a show action
- opens a websocket with the server
- that receives rendered messages on the `rooms_channel`
- and accepts messages to be broadcast to everyone else
- uses a background worker to render the message and broadcast it back to everyone

I've [transcribed it here](https://github.com/HappyFunCorp/actioncable_chatapp/blob/master/README.md) to see what was added.  The code is available to [clone the code from github](https://github.com/HappyFunCorp/actioncable_chatapp) if you don't have an app that you are working on already.

## Now lets talk about deployment

The app we generated was done using basic rails 5 commands, and it's out of the box stuff.  (Unlike most of the walkthroughs on this site which were done with [seed](http://seed.happyfuncorp.com).  It's setup to use puma (good), which will handle the WebSockets.  It's also setup with ActiveJob and ActionCable, both on which are in async local mode and won't use redis in development.  ActionCable will also need to know where it's websocket is going to connect to. Lets walk through everything that needs to get done.

## Gemfile

First we need to add some things to the Gemfile, specifically redis, postgres, and the sidekiq gems.  Add the following to your `Gemfile`

```rb
gem 'redis', '~> 3.0'
gem 'pg', group: :production
gem 'sqlite3', group: :development
gem 'sidekiq'
gem 'sinatra', git: 'https://github.com/sinatra/sinatra', :require => nil
```

Make sure you remove the `sqlite3` on the top of the file, we only want it in development.  We are installing `sinatra` out of the master branch since the current release doesn't play well with Rails 5.  (If it tries to install Sinatra 1.0 you'll get rake exception errors.)  If you don't want the web interface to sidekiq you can omit.

## Setup sidekiq

First we need to create a `Procfile` that defines our dynos

```
web: bundle exec puma -C config/puma.rb
worker: bundle exec sidekiq -c 2
```

I'm running sidekiq with a concurrency of 2 to limit the number of redis connections.  You'll probably want that a lot higher in real life if you are using Sidekiq for other things..

Now create `config/initializers/active_job.rb` and tell ActiveJob to use `sidekiq`

```rb
Rails.application.config.active_job.queue_adapter = :sidekiq
```

And let's add the sidekiq web interace to your `config/routes.rb`:

```rb
  require 'sidekiq/web'
  mount Sidekiq::Web => '/sidekiq'
```

If this was a real app, you'd limit who can get to that engine.  If you installed devise, the way to do that is:

```rb
  authenticate :admin_user do
    mount Sidekiq::Web => '/sidekiq'
  end
```

## A brief digression to test locally

It's always nice to change as little as we can, so when everything blows up in our face we can limit our investigations to a small change set.  So lets first migrate our development environment to communicate over redis, so the ActiveJobs are run in a seperate process, and that ActionCable will be able to communicate from that process, back to the main web server, back to the client.

Edit `config/cable.yml` to set the development environment use to use redis.

```
development:
  adapter: redis
  url: redis://localhost:6379/1
```

If you don't have redis installed, use [homebrew](http://brew.sh) to do that now:

```
$ brew install redis
$ redis-server
```

If you don't have foreman installed, do that now:

```
$ gem install foreman
```

Now lets run that app!

```
$ rails db:migrate
$ foreman start
```

Using foreman will start both the web interace as well as the sidekiq worker.

Now you should be able to go to `http://localhost:3000` in multiple windows and talk with yourself.  I really like talking with myself, and I assume that you do as well.

And `http://localhost:3000/admin` should load up the sidekiq admin console.  Validate that everything is working, since now we are going to spin up everything on heroku!

## Heroku and Redis

Create a new app:

```
$ heroku create
```

Then we create a redis instance

```
$ heroku addons:create heroku-redis:hobby-dev
```

We now need to tell ActionCable where it's redis server is.  Lets find out the answer and put in into the production section of `config/cable.yml`.

```
$  heroku config | grep REDIS
REDIS_URL:                redis://h:p75fl9as.........
```

`config/cable.yml`:

```
production:
  adapter: redis
  url: redis://h:p75fl9as.........
```

## Set the right ActionCable websocket address

We need to tell rails where it's expected to receive the websocket connection from. `heroku info` will show us our external application.  If you deploy on a custom url, you'll need to add that as well.

```
$ heroku info
=== aqueous-thicket-49913
Addons:        heroku-postgresql:hobby-dev
               heroku-redis:hobby-dev
Dynos:         web: 1, worker: 1
Git URL:       https://git.heroku.com/aqueous-thicket-49913.git
Owner:         operations@happyfuncorp.com
Region:        us
Repo Size:     34 KB
Slug Size:     29 MB
Stack:         cedar-14
Web URL:       https://aqueous-thicket-49913.herokuapp.com/
```

Lets tell rails about it.  Edit `config/environments/production.rb`.  Obviously update the urls for your application.

```
  config.action_cable.url = 'wss://aqueous-thicket-49913.herokuapp.com/cable'
  config.action_cable.allowed_request_origins = [ 'https://aqueous-thicket-49913.herokuapp.com']
```

Note that `wss` is WebSockets over over SSL.  You should use that, for a number of reasons including but not limited to security.  However, if you don't use SSL, use `ws` instead.

Now we need to tell the javascript where to connect.  Lets open up `app/views/layouts/application.html.erb` and add this into the `<HEAD>`:

```
 <%= action_cable_meta_tag %>
```

## Commit and push

First we add the code to repo

```
$ git add .
$ git commit -a -m "Added heroku configuration"
```

Now we push to heroku itself:

```
$ git push heroku master
```

If that goes well, lets create the database tables and spin up the worker process

```
$ heroku run rake db:migrate
$ heroku ps:scale worker=1
```

And now, lets run the app and look at the logs:

```
$ heroku open
$ heroku open
$ heroku log --tail
```

We ran open twice, so you could see what was there in two windows.  Does it work?  Do 3 windows work?

Do you like talking to yourself as much as I do?

## But does it scale

Right now you have 1 web dyno, running puma with 5 threads, and 1 worker dyno running sidekiq with 2 concurrent worker.  This should be at least 5 redis connections, up to 2 more depending upon how many jobs have gone through sidekiq.  Lets look at the redis info:

```
$ heroku redis:cli
ec2-54-243-230-243.compute-1.amazonaws.com:24949> info

....
# Clients
connected_clients:7
client_longest_output_list:0
client_biggest_input_buf:0
blocked_clients:2
....
```

So we see that we have 7 active connections now.  As you get more people listening to redis, those will go up.  Adding more dynos will push the number of active connections up, so you need to be careful in sizing both your postgres install as well as your redis install.


_Image credit [Lee Roberts](https://www.flickr.com/photos/flintman45/15521201146/in/photolist-pDydmj-4EmYXp-5iGRcG-4KrLFJ-rjnz22-9Cpo2C-azDiP-adi58f-aSgSYP-7Uc2y7-bmGghk-a4pD4T-5wDbbf-a3zri3-7rDRMh-9Z3c6a-471ZjP-8qQ2fX-kYdZ28-9Z6fc9-9ap1TR-2nhNUA-93VFQa-76jVr-4Q3WXY-bbVhbt-9Z3ij2-kx5BP4-59LXDM-nnav37-eTfHYC-p3JqVp-k6dTdF-qxURpr-ay5db9-5vk21b-oC9hPd-n4bS2m-476583-pMvQCn-CffNFE-rbndpL-6QP35H-d1mxVf-9Wrx53-a1LCGS-d1mxZJ-9ugrWA-9XFZPm-46WZVf)_
