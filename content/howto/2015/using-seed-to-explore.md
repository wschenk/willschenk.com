---
title: Using seed to explore APIs
subtitle: overview of what we're working on and how to explore apis
date: "2015-09-17"
tags:
  - happy_seed
  - rails
  - github
  - rake
aliases:
  - "/using-seed-to-explore/"
  - "/articles/2015/using-seed-to-explore/"
---

I've been working to update seed, which is HappyFunCorp's app generator to make it easy to kick off MVPs. Check out the website for more information. One of the things that I've started to do is to seperate out the dependancies more, and being tutorials on how to use each of the different features. After I link to that stuff, let's walk through a way to combine different techniques we've discussed together.

- Check out [the site](http://seed.happyfuncorp.com)
- Or watch the video:

<div class="embed-responsive embed-responsive-16by9">
  <iframe src="https://www.youtube.com/embed/hCsPDaKHw6I?rel=0" allowfullscreen></iframe>
</div>

## The basic process

1. Get access to the API.
2. Open up a console and start playing with commands.
3. Start storing the data locally once we need to get more.
4. Play around with the data.
5. Design a UI around in.
6. Refactor the test "scripts" into ruby classes that fit that UI.

## Using Seed, OAuth, and Rake to explore github data

What I want to do is to look at all of the `Gemfile`s for all of our projects, and see which gems are the most popular, which versions we are using, and if we can develop some more expertise around it. But first, I want to get the data.

I could go to each of the repos in github, but there are well over a hundred so that doesn't work. Github has an API, which is great, but I need oauth2 to access it. Let's get going.

## Creating a github application

The first step is to make sure you have a github application.

1. Go to [developer applications](https://github.com/settings/developers)
2. Click _Register New Application_
3. Enter in `http://localhost:3000/users/auth/github/callback` as the callback URL.
4. Fill in whatever else.
5. _Resister the Application_

At this point you should have the _Client ID_ and the _Client Secret_.

## Create a seed_defaults file

Once I create a remote app, I like to put the credentials into the `~/.seed_defaults` file. This will make it so the next time I generate a seed app, these are the default credentials that are used.

```bash
GITHUB_APP_ID=replace with client id
GITHUB_APP_SECRET=replace with client secret
```

## Generate the app

If you haven't installed [happy_seed](http://seed.happyfuncorp.com) yet, do so now by typing `gem install happy_seed`.

Then generate the app:

```bash
$ happy_seed rails project_stats
```

Now it's going to install things. Inititally, just say no to everything. Once it's done, install the github generator:

```bash
$ cd project_status
$ rails g happy_seed:github
```

That will do its thing. Then we need to create the database and get going:

```bash
$ rake db:migrate
```

## Ask for more scope

When the _github_ generator is run, it configures the oauth scope that it requests in `config/initializers/devise.rb`. We need to ask for a bit more permissions, so open up that file and change the scope requested to be `"user,repo,read:org"`, so:

```vash
config.omniauth :github, ENV['GITHUB_APP_ID'], ENV['GITHUB_APP_SECRET'], scope: "user,repo,read:org"
```

Now lets start the server and see what's up:

```bash
$ rails s
```

1. Point your browser to http://localhost:3000
2. Select "Sign up" from the "Account" menu.
3. Press "Sign in with Github"
4. You should be bounced to github and you have to accept.
5. You should be back on localhost with the message `Successfully authenticated from Github account.`

## Lets play

1. ~~Get access to the API.~~
2. Open up a console and start playing with commands.
3. Start storing the data locally once we need to get more.
4. Play around with the data.
5. Design a UI around in.
6. Refactor the test "scripts" into ruby classes that fit that UI.

Lets get in that console and see what we can do. If you look at `app/models/user.rb` you can see that there's code to setup the github client, and we can access it like so:

```bash
$ rails c
Loading development environment (Rails 4.2.3)
2.2.1 :001 > gc = User.first.github_client
  User Load (0.2ms)  SELECT  "users".* FROM "users"  ORDER BY "users"."id" ASC LIMIT 1
  User Load (0.2ms)  SELECT  "users".* FROM "users"  ORDER BY "users"."id" ASC LIMIT 1
  Identity Load (0.4ms)  SELECT  "identities".* FROM "identities" WHERE "identities"."user_id" = ? AND "identities"."provider" = ?  ORDER BY "identities"."id" ASC LIMIT 1  [["user_id", 1], ["provider", "github"]]
  Identity Load (0.4ms)  SELECT  "identities".* FROM "identities" WHERE "identities"."user_id" = ? AND "identities"."provider" = ?  ORDER BY "identities"."id" ASC LIMIT 1  [["user_id", 1], ["provider", "github"]]
 => #<Octokit::Client:0x007fe1f7bec7e8

2.2.1 :002 > gc.org_repos( 'HappyFunCorp', {:type => 'private'} ).count
 => 30
```

Ok, now we can start figuring out what we need to do to get access to the data. We have an authenticated user account, and we can start hitting the API. I know for a fact that I have way more than 30 repos -- I mean, seriously -- so first thing is to figure out why that is and how to get more. It's probably related to pagination.

```ruby
2.2.1 :003 > gc.org_repos( 'HappyFunCorp', {:type => 'private', per_page: 100} ).count
 => 100
```

OK, looking through the [octokit issues](https://github.com/octokit/octokit.rb/issues/255) this can be dealt with by turning `auto_paginate: true` on when we load up the client. So let's edit `app/models/user.rb` to do that:

```ruby
@github_client ||= Octokit::Client.new(access_token: github.accesstoken, auto_paginate: true)
```

Back to the console, do `reload!` and load up our client again. Notice that I'm making this a one liner, since we're going to be doing it over and over its nice to use the arrow keys.

```ruby
2.2.1 :004 > reload!; gc = User.first.github_client
Reloading...
  User Load (0.1ms)  SELECT  "users".* FROM "users"  ORDER BY "users"."id" ASC LIMIT 1
  User Load (0.1ms)  SELECT  "users".* FROM "users"  ORDER BY "users"."id" ASC LIMIT 1
  Identity Load (0.2ms)  SELECT  "identities".* FROM "identities" WHERE "identities"."user_id" = ? AND "identities"."provider" = ?  ORDER BY "identities"."id" ASC LIMIT 1  [["user_id", 1], ["provider", "github"]]
  Identity Load (0.2ms)  SELECT  "identities".* FROM "identities" WHERE "identities"."user_id" = ? AND "identities"."provider" = ?  ORDER BY "identities"."id" ASC LIMIT 1  [["user_id", 1], ["provider", "github"]]
 => #<Octokit::Client:0x007fe1f7d986f0 @access_token="*.......

2.2.1 :005 > gc.org_repos( 'HappyFunCorp', {:type => 'private'} ).count
 => 169
```

OK, that looks better. That will give us a list of all the repos, so now we just need to see how to get the contents of our file, and then we can put it all together.

```ruby
2.2.1 :006 > repo = gc.org_repos( 'HappyFunCorp', {:type => 'private'} ).first
 => {:id=>988713,
 :name=>"benchcoach",
 :full_name=>"HappyFunCorp/benchcoach",
 :owner=>
 .....

2.2.1 :007 > repo.full_name
 => "HappyFunCorp/benchcoach"

2.2.1 :008 > content = gc.contents repo.full_name, path: 'Gemfile'
 => {:name=>"Gemfile",
 :path=>"Gemfile",
 :sha=>"1d67bb85e43adab5b68ecc0eb3a5304e6ee2588e",....
```

Looking at this, we see that github returns the contents of the file base64 encoded. I guess that makes sense, so if we want to print it out:

```ruby
2.2.1 :009 > Base64.decode64 content.content
```

## Using Rake to pull the data down

1. ~~Get access to the API.~~
2. ~~Open up a console and start playing with commands.~~
3. Start storing the data locally once we need to get more.
4. Play around with the data.
5. Design a UI around in.
6. Refactor the test "scripts" into ruby classes that fit that UI.

Lets use rake to start managing the data. We're going to be using some of the techniques that were outlined in the [using rake for dataflow programming and data science](/using-rake-for-dataflow-programming-and-data-science/]) post. First step is to create a `lib/tasks/github.rake` file that we're going to put our tasks.

```ruby
require 'fileutils'

file "data/projects.json" do
  Rake::Task["environment"].invoke
  FileUtils.mkdir_p "data"

  gc = User.first.github_client

  data = gc.org_repos( 'HappyFunCorp', {:type => 'private'} ).collect do |x|
    { name: x[:name],
      full_name: x[:full_name],
      url: x[:url] }
  end

  File.open( "data/projects.json", "w" ) do |out|
    out.puts JSON.unparse( data )
  end
end
```

_Be sure to change the `HappyFunCorp` to your organization, or use the `repos` call instead of the organization one._

Now lets run `rake data/projects.json`. If you run it a second time, notice that rake returns imediately and doesn't hit the remote server.

1. The `file` task only runs if the file doesn't exist.
2. `Rake::Task["environment"].invoke` is a way to ensure that a task as been run without forcing it to run.
3. The API calls are from our console experiments.
4. Just save it to a file.

OK, now lets be able to loop over everything to load the files that we want. First we define a method that lets us define a task to loop over all the entries in a JSON array, and then we'll call it with our block which loads up the contents. (Add this to the end of the `github.rake` file)

```ruby
def for_each_elem( name, file )
  task name => file do
    JSON.parse( File.read( file ) ).each do |record|
      yield record
    end
  end
end

for_each_elem "load_gemfiles", "data/projects.json" do |repo|
  outfile = "data/gemfiles/#{repo['name']}.Gemfile.lock"
  FileUtils.mkdir_p "data/gemfiles"

  file outfile do
    Rake::Task["environment"].invoke

    gc = User.first.github_client

    begin
      content = gc.contents repo['full_name'], path: 'Gemfile.lock'

      File.open outfile, "w" do |out|
        out.puts Base64.decode64 content.content
      end
    rescue Octokit::NotFound
      puts "No Gemfile.lock found for #{repo['full_name']}"
    end
  end

  Rake::Task[outfile].invoke
end
```

And run it, `rake load_gemfiles`. Depending upon how many repos you have, this could take a few seconds. (Also make sure you've updated the organization!)

1. Define a file task for each output file, that we will `invoke` at the very end.
2. Inside the task, make sure that the `environment` is loaded.
3. Pull down the contents of the Gemfile.lock from the API.

If you run this a second time, notice that it only attempts to load from the files that weren't loaded before.

For fun, delete the `data` directly and run the rake task again. BOOM!

## Massaging the data into something usable

1. ~~Get access to the API.~~
2. ~~Open up a console and start playing with commands.~~
3. ~~Start storing the data locally once we need to get more.~~
4. Play around with the data.
5. Design a UI around in.
6. Refactor the test "scripts" into ruby classes that fit that UI.

OK, now that we have all the data, lets figure out how to slice and dice it. Lets just wire together some standard UNIX tools to filter and get some info.

```ruby
for_each_elem "filter_gemfiles", "data/projects.json" do |repo|
  sourcefile = "data/gemfiles/#{repo['name']}.Gemfile.lock"
  outfile = "data/filtered/#{repo['name']}.gems"
  FileUtils.mkdir_p "data/filtered"

  if File.exists?( sourcefile )
    file outfile do
      system( "awk '/^    [^ ]/ { print $1, $2 }' #{sourcefile} > #{outfile}" )
    end
    Rake::Task[outfile].invoke
  end
end
```

Running `rake filter_gemfiles` will go through and only show the specific gems that were locked out the Gemfile.locks. Obviously, filtering the file based on the fact that it has exactly 4 spaces isn't robust, but it works.

Lets add a couple of other nifty methods:

```ruby
file "data/versioned.list" do
  system( "cat data/filtered/* | sort | uniq -c | sort -rn > data/versioned.list" )
end

file "data/gems.list" do
  system( "awk '{print $1}' data/filtered/* | sort | uniq -c | sort -rn > data/gems.list")
end
```

I'm going to stop here, but in case you are wondering the top gems that we use are:

1. (82) json
2. (81) tzinfo
3. (81) i18n
4. (81) activesupport
5. (79) rack
6. (79) multi_json
7. (78) sass
8. (77) rack-test
9. (76) tilt
10. (76) mime-types

## Repeatable data in 10 minutes

There's lots of stuff you can do from there, the most likely one being "sending an email and forgetting about it." But lets look at what we have.

1. The access key isn't hard coded anywhere. When you come back to this, if it expires, you just reconnect on the website.
2. Way easier to get access keys this way, only a few oauth providers make this simple. (Twitter does, for example, github doesn't.)
3. There's a direct process transitioning from 'playing around' to automated.
4. Loading the data from the remote API is automated and repeatable. If you've setup the dependancies correctly, you can run the rake tasks and things magically get up to date.
5. If you do want to build a UI around this, you already have a webapp up and running...

Importantly, this is something that you can get up and going with in under 10 minutes, at least if you know how the API works. It takes less that 1 minute to get to the point where you have an authenticated client to the remote service and you can spend time exploring.

One of the reasons I like having seed around to help prototype and explore ideas!

Source code can be found: https://github.com/wschenk/project_stats_demo
