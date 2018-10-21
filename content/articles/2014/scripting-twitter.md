---
title: 'Scripting Twitter: Collecting Data and Writing Bots'
subtitle: adding another client to socialinvestigator
tags:
  - ruby
  - socialinvestigator
  - bots
  - howto
date: 2014-11-20
header_image: robots.jpg
obsolete: true
aliases:
  - "/scripting-twitter/"
---
Lets build on our [command line url exploring tool](http://willschenk.com/making-a-command-line-utility-with-gems-and-thor) to look at how we can interact with Twitter.  We are going to cover how to make a script that will pull information out of twitter, how to deal with its rate limiting, and how to interact with users on Twitter itself.

Twitter uses [OAuth 1.0A](https://dev.twitter.com/oauth/overview/faq) as a way to authenticate requests.  As a script writer, this is super annoying, because you can't just stick a username and password in the environment and go from there.  You need to:

1. [Register you application with twitter](https://apps.twitter.com).
2. Store the "client key" for your application.
3. Get a user to grant your application access to twitter, and then use that "authorization key" to access the API.
4. Then figure out how to use the API.

This is a bit overkill for building a simple script, especially since the 3 way process of having your application request access on behalf of a specific user on the twitters servers is a pain for a command line interface.  And when you configure your application on twitter you need to specify a "callback URL", which a) is different on development, staging and production webapp environments and b) you don't have a web app.

Luckily for you twitter has a work around.

## Firstly, we create the Twitter App

1. Go to [Create new Twitter App](https://apps.twitter.com/app/new) and fill out the required fields.  Specifically, leave _callback URL_ blank for now.
2. Select _Keys and Access Tokens_.  On this page, create an access token for your user with the _Generate Access Token_ on the bottom of the page.

Now we have four variables that our script needs to access twitter on behalf of your user.  

1. The consumer key represents your application.
2. The consumer secret represents your application's "password".
3. The access token represents an authorization that a user has accepted.
4. The access token secret is there to make sure that access token is valid.

Make note of these variables.

## Then, we code with the REST API

Make sure you have the twitter gem installed by typing:

```bash
$ gem install twitter
```

We're eventually going to add this code our CLI gem, and that dependency will be created by adding the line

```ruby
  spec.add_dependency 'twitter'
```

OK, lets put this in and see what we get:

```ruby
require 'thor'
require 'twitter'
require 'httparty'

TWITTER_APP_KEY="rzlJXhI8..."
TWITTER_APP_SECRET="euF5bItp9w..."
TWITTER_ACCESS_TOKEN="17827..."
TWITTER_ACCESS_TOKEN_SECRET="7NSX..."

def print_user_info( u )
  t = "%-20s: %s\n"
  printf t, "Screenname", u.user_name
  printf t, "Full Name", u.name
  printf t, "Bio", u.description
  printf t, "Website", u.website.to_s
  printf t, "Joined", u.created_at.strftime( "%Y-%m-%d" )
  printf t, "Location", u.location
  printf t, "Verified", u.verified?
  printf t, "Tweets", u.tweets_count
  printf t, "Followers", u.followers_count
  printf t, "Following", u.friends_count
  printf t, "Favorites count", u.favorites_count
end

class CLI < Thor
  desc "user SCREENAME", "Look up info for a specific user."
  def user( username )
    print_user_info client.user( "wschenk" )
  end

# We will add more methods here

  private
  def client
    @client ||= Twitter::REST::Client.new do |config|
      config.consumer_key        = TWITTER_APP_KEY
      config.consumer_secret     = TWITTER_APP_SECRET
      config.access_token        = TWITTER_ACCESS_TOKEN
      config.access_token_secret = TWITTER_ACCESS_TOKEN_SECRET
    end
  end
end

# Only run if the script is called directly
if __FILE__ == $0
  CLI.start( ARGV )
end
```

When we run this, we get:

```bash
$ ruby lib/socialinvestigator/client/twitter.rb wschenk
Screenname          : wschenk
Full Name           : Will Schenk
Bio                 : Co-Founder of HappyFunCorp.
Website             : http://t.co/OA0tQaQiAX
Joined              : 2006-12-22
Location            : Brooklyn NY
Verified            : false
Tweets              : 1674
Followers           : 330
Following           : 418
Favorites count     : 17
```

_For those keeping score at home, that's 209 tweets a year._

We create a `Twitter::REST::Client`, passing in the variables that we got when we created the twitter app.  Eventually we'll out grow hardcoding them into the script, but for now replace the dummy values I have above with what you have and try it with some screen names.

## Longing for OA0tQaQiAX

If there's one word that describes URL shorteners, that word is _rude_.  This magical place where anyone can setup shop and link directly to other people now is now infested with middle men, who may indeed be useful but are even less popular than used car salesmen.  URL shorteners are services that _intermediate_ the actual destination of the link in order to better track who clicks on the link.  You create many shortened links and give them out to different people, and when the world at large clicks on one of them you know who they got it from.

The easiest way to resolve the link is to make a `HEAD` HTTP request to the shortening service and print where they redirect you to.  If there's no location in the response, we'll just return the url that was passed in.

```ruby
def lookup_url( url )
  return url if url.nil? || url == ""
  r = HTTParty.head url, { follow_redirects: false }
  r['location'] || url
end
```

and lets add a Thor command for it too, in the `CLI` class, why not:

```ruby
  desc "lookup URL", "Resolve a link"
  def lookup( url )
    puts lookup_url( url )
  end
```

Now we can see that

```bash
$ ruby twitter.rb lookup  http://t.co/OA0tQaQiAX
http://happyfuncorp.com
$ ruby twitter.rb lookup  http://happyfuncorp.com
http://happyfuncorp.com
```

So we can update the website `printf` line to be:

```ruby
  printf t, "Website", lookup_url( u.website.to_s )
```

## Tweets timelines mentions retweets

With read access, you can pull down some basic stuff.

Load the recent tweets this user has made:

```ruby
  desc "user_timeline", "Show the authenticated user's tweets"
  def user_timeline
    client.user_timeline.each do |tweet|
      puts "@#{tweet.user.user_name}:#{tweet.text}"
    end
  end
```

Show the tweets of people who they are following:

```ruby
  desc "home_timeline", "Show the authenticated user's timeline"
  def home_timeline
    client.home_timeline.each do |tweet|
      puts "@#{tweet.user.user_name}:#{tweet.text}"
    end
  end
```

Show which of their tweets have been retweeted:

```ruby
  desc "retweets", "Show the authenticated user's retweets"
  def retweets
    client.retweets_of_me.each do |tweet|
      puts "@#{tweet.user.user_name}:#{tweet.text}"
    end
  end
```

Pull down only the tweets that mention the authentication user:

```ruby
  desc "mentions", "Show the authenticated user's mentions"
  def mentions
    client.mentions.each do |tweet|
      puts "@#{tweet.user.user_name}:#{tweet.text}"
    end
  end
```

If you are building a bot, for example, that you wanted to respond when someone tweets at it, you could use the `client.mentions` method to get those tweets in particular.  You'd need a way to make sure you don't double respond to people each time the bot was run.  If you wanted to always have the bot running, look at the Streaming API below.  If you want to know how to post to twitter, read on.

## Rate Limits on the REST interface

Unlike Google, who casually has enough computer power to do personalized type ahead search of the _entire internet_ at _a global scale_, Twitter is prickly about [you hammering their site](https://dev.twitter.com/rest/public/rate-limiting).  Personally, I thought that [the fail whale](http://readwrite.com/2008/07/17/the_story_of_the_fail_whale) was really fun.  Let's add a another `CLI` command to print out the current rate limit status, how many calls you have left, and when your counter for that resource will reset:

```ruby
  desc "limits", "Print out the current rate limits"
  def limits
    resp = client.get( "/1.1/application/rate_limit_status.json" )
    current_time = Time.now.to_i
    template = "   %-40s %5d remaining, resets in %3d seconds\n"
    resp.body[:resources].each do |category,resources|
      puts category.to_s
      resources.each do |resource,info|
        printf template, resource.to_s, info[:remaining], info[:reset] - current_time
      end
    end
  end
```

Note that I'm making the request using `client.get` which will make an arbitrary authenticated request to the Twitter api.  This [particular api call](https://dev.twitter.com/rest/reference/get/application/rate_limit_status) isn't in the Twitter gem, though the gem is aware of the limits and will throw specific exceptions when you hit those limits.

The gem provides methods on top of this `client.get` interface, which may use more API calls then you expect.  Methods like `client.user_timeline` and `client.followers` return `Twitter::Cursor` objects, which you can iterate over in ruby as you'd expect, but may trigger multiple API calls throughout the process which you might not expect.  You'll potentially get an exception in the middle of things, and you'll need to figure a way around this.  In a later post we will get into caching and retry strategies, but since it will dramatically increase the complexity we'll keep things simple for now.  _Punt!_

This basic code will catch the `TooManyRequests` exception and sleep the process until it's ready to go again.  This could take a very very long time.

```ruby
follower_ids = client.follower_ids('justinbieber')

begin
  follower_ids.to_a
rescue Twitter::Error::TooManyRequests => error
  # NOTE: Your process could go to sleep for up to 15 minutes but if you
  # retry any sooner, it will almost certainly fail with the same exception.
  sleep error.rate_limit.reset_in + 1
  retry
end
```

## Listing followers

To see what that looks like, lets add another `CLI` command for listing a person's followers:

```ruby
  desc "followers SCREENNAME", "Prints out all of the users followers"
  def followers( screenname )
    client.followers( screenname ).each do |u|
      printf( "@%-15s %-20s %s\n", u.user_name, u.name, u.description )
    end
  end
```

And if we run this on someone will a lot of followers, you'll see the `Twitter::Error::TooManyRequests` thrown.  We can look use our limits command above to see how long we have to wait until it resets.  Though, since we don't have any smart retrying logic in place, chances are it will still fail when we try it again.

```bash
$ ruby twitter.rb limits | grep /followers/list
   /followers/list        0 remaining, resets in 288 seconds
```

## Searching for URLs

There are different types of URLs on twitter.  Lets look at a specific tweet of mine, 529342690476179456, to see what there is:

```ruby
> tweet = client.status( 529342690476179456 )
 => #<Twitter::Tweet id=529342690476179456>
> tweet.uris
 => [#<Twitter::Entity::URI:0x007ff614b0fce0 @attrs={:url=>"http://t.co/frfwwIqrYB", :expanded_url=>"http://willschenk.com/bootstrap-advanced-grid-tricks", :display_url=>"willschenk.com/bootstrap-adva…", :indices=>[67, 89]}>]
```

`:url` is the actual link that get's clicked, and in your logs, the one that will show up as the referer.

`:expanded_url` is the end link.

`:display_url` is a shortened version of the end link.

In the twitter search box we can type in `http://willschenk.com/bootstrap-advanced-grid-tricks` and that will match all tweets that go to this URL, regardless of which URL shortening service they use.  (To clarify, all those services which Twitter has support for, which for our purposes is all.)  If we type in `http://t.co/frfwwIqrYB` to the twitter search it won't match the tweet, but if we do _an exact search_, with quotes around it like so: `"http://t.co/frfwwIqrYB"` we will match the tweet.

Let's write some code:

```ruby
  desc "search STRING", "Shows all the tweets that match the string"
  options [:exact, :user_info]
  def search( string )
    string = "\"#{string}\"" if options[:exact]
    reach = 0
    client.search( string, count: 100 ).each do |t|
      puts "#{t.id}:#{t.created_at}:@#{t.user.user_name}:#{t.user.followers_count}:#{t.retweet_count}:#{t.text}"
      reach += t.user.followers_count
      if options[:user_info]
        print_user_info t.user if options[:user_info]
        puts
      end
    end
    puts "#{string} reached #{reach} people."
  end
```

This will print out the tweet id, when it was created, who tweeted it, how many followers they had, how many times it was retweeted, and the text of the tweet.  _Whew!_ Since there's so much stuff here we made it `:` separated on one line, in case you want to parse it with something else.

The option `--exact` will put quotes around the search string, so if you are looking up a tweet from your referrers you can find which tweet sent people to your site.

The option `--user_info` will print out the full information about the user who tweeted.

Twitter search only returns results from the previous 6-9 days, so better act fast!

## The Streaming API: Filters

So far we have been looking at the REST API, which lets us interact with the Twitter services much like a user would: in response to something that the user requests.  The other API type is called the [streaming api](https://dev.twitter.com/streaming/userstreams), which lets us open up a single connection and receive information when Twitter has something new.  This is useful for a few things:

1. We can get notified immediately when our search term is seen
2. We can get notified if someone mentions us, either with an @reply or, when we have more permissions, with Direct Messages.

Here's how you can watch twitter for different terms, they can be comma separated.  Notice that we've added another private method, `streaming_client` which uses a different interface, and we'll need to `^C` the script to start it.

The method `filter` takes a block, and will call that block every time it gets a response from twitter.

```ruby
  desc "filter TERMS", "Print out tweets that match the terms"
  def filter( terms )
    streaming_client.filter(track: terms) do |object|
      puts "@#{object.user.user_name}:#{object.text}" if object.is_a?(Twitter::Tweet)
    end
  end


  private
  [...]

  def streaming_client
    @streaming_client ||= Twitter::Streaming::Client.new do |config|
      config.consumer_key        = TWITTER_APP_KEY
      config.consumer_secret     = TWITTER_APP_SECRET
      config.access_token        = TWITTER_ACCESS_TOKEN
      config.access_token_secret = TWITTER_ACCESS_TOKEN_SECRET
    end
  end
```

## The Streaming API: Reacting to the timeline

We can also watch the timeline come through for the registered user, and do something with it.  The pattern is the same as the `filter` method, but it passes an Object of one of the following types:

- `Twitter::Tweet`
- `Twitter::DirectMessage`
- `Twitter::Streaming::DeletedTweet`
- `Twitter::Streaming::Event`
- `Twitter::Streaming::FriendList`
- `Twitter::Streaming::StallWarning`

We'll be ignoring most of them:

```ruby
  desc "listen", "Prints our the authenticated user's stream as it happens"
  def listen
    streaming_client.user do |object|
      case object
      when Twitter::Tweet
        puts "Tweet:@#{object.user.user_name}:#{object.text}"
      when Twitter::DirectMessage
        puts "DM:@#{object.sender.user_name}:#{object.text}"
      when Twitter::Streaming::StallWarning
        warn "Falling behind!"
      end
    end
  end
```

Running this command will print out tweets and messages as they come in.  If you wanted to make an interactive bot, you'd put logic in here to respond to the tweets coming in, calling something like

```ruby
client.update( "@#{object.user.user_name} my reply", { in_reply_to_status: object.id } )
```

Only, you're not going to get any direct messages, nor are you going to be able to reply, unless you change your application permission settings.

## Upgrading to the All Access Pass

In order to

- Update a status
- Read direct messages
- Send direct messages

You need to change the application permission level.  There are three different levels.  Read will let you pull things on behalf of the user.  If they have a private account, or they have access to a private account, your script will be able to see those things.  You will not be able to access or receive direct messages

Write will let you post public status updates, but not Direct Messages.

Access direct messages is the third.

1. Go to the [twitter app console](https://apps.twitter.com) and find your application.
2. Go to the "Permissions Tab"
3. Change the settings to "Read, Write" or "Read, Write and Access direct messages"
4. Go to "Keys and Access Settings"
5. Scroll down and "Regenerate My Access Token and Token Secret"

Depending upon what you chose, you can now post on behalf of the user:

```ruby
client.update( "Hello, World" )
```

Read their direct messages:

```ruby
client.direct_messages
```

Or send a direct message:

```ruby
client.create_direct_message "wschenk", "Hi there"
```

## The Code

Here is the [code written for this article](https://gist.github.com/wschenk/86e1e87772e7d589e963).  The github for [socialinvestigator](https://github.com/sublimeguile/socialinvestigator) has a more complete sample that loads and stores the credentials in a file.

_Image Credit [Steve Jurvetson](https://www.flickr.com/photos/jurvetson/7408451314/in/photolist-bvcbis-7DN6Sk-7DRVeq-7DRUVq-jB3M44-dhZrKM-atAnS1-bi4yL6-atHJJo-85tMTG-cp5WgS-asg29e-chEftd-6Zex9h-djkSiW-acqro-c7PG79-72SJuQ-7fPiox-avgwDx-bEWnc2-4FAqy9-9mvMsP-8fp27H-chEjh3-66UgvR-6oqRAV-6oqUMD-5zWDAn-9mqRC4-6ov3Mw-6oqRQZ-6ov4bh-avgxEZ-6VXNYF-avgwMM-avjcLf-avjcQ1-avjcLS-avjcPd-avjcTW-avjduE-avgwZK-avjdMm-avjcVy-avjcSU-avgwRt-avjcY1-avjd2d-avgwTv)_
