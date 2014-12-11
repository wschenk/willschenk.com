---
:title: Pulling data out of Google Analytics
:subtitle: see who's talking about your stuff
:tags: socialinvestigator, howto, ruby
:header_image: lava-lamp.jpg
:date: 2014-12-04
---
I like staring at the real time stats of [Google Analytics](http://www.google.com/analytics/).  As a dashboard, it's not really as amazing as [Chartbeat](https://chartbeat.com) is, and it doesn't let you drill down into the data as much as [Mixpanel](https://mixpanel.com).  But GA is super simple to setup and it's Google, so everyone uses it.

Another obsessive/fun thing to do is to see where that spike in inbound traffic is coming from.  On [HappyFunCorp](http://happyfuncorp.com) there are days where we get a sudden influx of [Happy Thoughts](http://happyfuncorp.com/#happy-thoughts) which warms our hearts and floods our inboxes.  Where did they come from?  How do we figure it out?

Lets look at how we can interact with Google Analytics using [google-api-ruby-client](https://github.com/google/google-api-ruby-client).  At the end of this, we are going to be able to see the current traffic stats, top referrals, see a timeline of when the referals first came in, and do what we can from that information to track down who is talking about us.  GA will show us that we are getting a lot of `SOCIAL` traffic, but what else can we figure out?

## Step 1 Setting it up to access Google on behalf of the user

We're going to be using OAuth2 to authorize our script.  So head over to the [Google Developers Console](https://console.developers.google.com/project).

1. *Create a Project*.  You should name this something that makes sense to you.  I called my _Social Investigator_

2. Enable the _Analytics API_.  This can be done on the side bar, under **APIs and auth > Apis**.  Scroll down to find it.

3. **APIs and auth > Consent Screen**.  Create something here, you'll need to flesh this out later.

4. **APIs and auth > Create Client ID**.  Select **Installed Application** with type _Other_.  This will create the keys for you, and then you **Download JSON** and save it in a file.

## Step 2 Getting an access token and an API file using InstalledAppFlow

Working with the Google API is pretty confusing at first, since there's multiple steps that need to happen before you can even figure out how to make a call.  Twitter's API, which in every other way is a joke compared to Google's way of doing things, has a [handy way to get a single use access token](/scripting-twitter).  With Google you need to do this yourself.  And once that's done, you need to load the API meta data from the API API to be able to access it!

We're going to be using a few gems to put in your `Gemfile` specifically:

```rb
source 'https://rubygems.org'

gem 'google-api-client'
gem 'thor'
gem 'hirb'
```

Now we can use the `Google::APIClient::InstalledAppFlow` class to open up a web browser, have the user log in as needed to their Google Accounts, and grant access to the API.  The code below shows the basics of this.  We assume that the file you downloaded in step 1 is called `client_secrets.json` and in the same directory, and we are writing out the granted credentials into the `analytics-oauth2.json` file.

```rb
#!/usr/bin/env ruby -KU
#
# This code has been adapted from
# https://github.com/google/google-api-ruby-client-samples/tree/master/drive
#

require 'thor'
require 'hirb'
require 'google/api_client'
require 'google/api_client/client_secrets'
require 'google/api_client/auth/file_storage'
require 'google/api_client/auth/installed_app'
require 'logger'
require 'csv'

API_VERSION = 'v3'
CACHED_API_FILE = "analytics-#{API_VERSION}.cache"
CREDENTIAL_STORE_FILE = "analytics-oauth2.json"
CLIENT_SECRETS_FILE = "client_secrets.json"

class AnalyticsClient
  def initialize( file_caches = nil )
    client
  end

  def client
    return @client if @client

    @client = Google::APIClient.new(:application_name => 'Analyics-CLI',
        :application_version => '1.0.0')

    # FileStorage stores auth credentials in a file, so they survive multiple runs
    # of the application. This avoids prompting the user for authorization every
    # time the access token expires, by remembering the refresh token.
    # Note: FileStorage is not suitable for multi-user applications.
    file_storage = Google::APIClient::FileStorage.new(CREDENTIAL_STORE_FILE)
    if file_storage.authorization.nil?
      client_secrets = Google::APIClient::ClientSecrets.load(CLIENT_SECRETS_FILE)
      # The InstalledAppFlow is a helper class to handle the OAuth 2.0 installed
      # application flow, which ties in with FileStorage to store credentials
      # between runs.
      flow = Google::APIClient::InstalledAppFlow.new(
        :client_id => client_secrets.client_id,
        :client_secret => client_secrets.client_secret,
        :scope => ['https://www.googleapis.com/auth/analytics']
      )
      @client.authorization = flow.authorize(file_storage)
    else
      @client.authorization = file_storage.authorization
    end

    @client
  end
end

if __FILE__ == $0
  AnalyticsClient.new.client
end
```

When we run this the first time, you should be prompted to grant access to your application.  The second time it should run and exit cleanly -- it has access, but we haven't asked to do anything yet.

## Step 3 Discover the API

Google takes their software development seriously, and it shows.  Not only are there many different APIs available to use, but they all have different versions.  These endpoints are all different, and rather than have them all hard coded into the client access library, you use the _discover api_ to pull in metadata associated with it.  The following code will load up this data and cache it to the filesystem so the next access will be faster.

```rb
  def api
    return @api if @api

    # Load cached discovered API, if it exists. This prevents retrieving the
    # discovery document on every run, saving a round-trip to API servers.
    if File.exists? CACHED_API_FILE
      File.open(CACHED_API_FILE) do |file|
        @api = Marshal.load(file)
      end
    else
      @api = client.discovered_api('analytics', API_VERSION)
      File.open(CACHED_API_FILE, 'w') do |file|
        Marshal.dump(@api, file)
      end
    end

    @api
  end
```
## Step 4 Finding a web profile

In order to pull data from an analytics account, you need to query the management API to get a list of profiles that you user has access to.  We're going to collapse the differences between accounts and properties, and print them all out in a list directly.  The key variable we are looking for is going to be the profile _id_.  This is different from the _web property id_, which is what you use in Javascript to add the tracking code (.e.g `UA-56296045-1`).  We'll also show the _websiteUrl_ associated with the account since that's what people really know.


```rb
  def profiles
    client.execute(
      api_method: api.management.profiles.list,
      parameters: { accountId: "~all", webPropertyId: "~all" } )
  end

  def print_profiles
    profiles.data.items.each do |profile|
      printf "%-15d %-15s %s\n", profile.id, profile.webPropertyId, profile.websiteUrl
    end
  end
end

if __FILE__ == $0
  AnalyticsClient.new.print_profiles
end
```

## Step 5 Querying with ga.get

The main end point we are looking at is `ga.get`.  There's an [interactive developer tool](https://ga-dev-tools.appspot.com/explorer/) that will let you experiment with what is available and how it works.   If you load up that tool now, you'll see what we've written code that will let us find the property id for our query, so we are now ready to start querying.

The _Query Explorer_ is really useful because of the dropdowns around _dimensions_ and _metrics_.  When you hover over any of the fields, documentation comes up which will explain what each of the fields mean.

_Dimensions_ are different ways of slicing up the data.  These include things like _page title_, _referer_, _adwords_, and other ways of slicing up the people that came to your site.  _Metrics_ are the actual data for these buckets of users, and include things like _sessions_, _user counts_, _page views_ and _average session duration_.

In order to make the query we first need to setup the query parameters.  We've split that off into its own method called `query_template`.  The required fields are _profile id_, _start date_, and _end date_.  We're going to setup some defaults here which we will override in other methods when we use it.

```rb
  def query_template( profile_id, start_date = nil, end_date = nil )
    today = Time.now.strftime( "%Y-%m-%d" )
    {
      "ids" => "ga:#{profile_id}",
      "start-date" => start_date || today,
      "end-date" => end_date || today,
      "sort" => "-ga:pageviews",
      "dimensions" => "ga:pageTitle",
      "metrics" => "ga:pageviews,ga:newUsers,ga:users"
    }
  end
```

Our default here is that we're slicing on `pageTitle`, showing `pageviews`, `newUsers`, and `users` (which include returning users).

Let's do an actual query with the parameters:

```rb
  def hotcontent( profile_id, start_date = nil, end_date = nil )
    query = query_template( profile_id, start_date, end_date )
    client.execute(
      api_method: api.data.ga.get,
      parameters: query
      )
  end
```
Now we need a way to print the result.  The result that we get back has two different things, `columnHeaders` which is a reflection of the `query` that we passed in, and the data itself is an array of arrays in `row`.  We're using ` Hirb` helper method here to format the result.

```rb
  def print_query_result( r )
    headers = r.data.columnHeaders.collect { |x| x.name }
    puts Hirb::Helpers::AutoTable.render(r.data.rows, headers: headers )
  end
```

Let's give it a try:

```rb
if __FILE__ == $0
  client = AnalyticsClient.new
  results = client.hotcontent ARGV[0]
  client.print_query_result results
end
```
Make sure you've made note of your profile id above, and we can see what it looks like now:

```sh
$ ruby ga.rb 93249816
+--------------------------------------------------+--------------+-------------+----------+
| ga:pageTitle                                     | ga:pageviews | ga:newUsers | ga:users |
+--------------------------------------------------+--------------+-------------+----------+
| Building Sites with Middleman | Will Schenk      | 26           | 11          | 24       |
| Bootstrap: Advanced Grid Tricks | Will Schenk    | 11           | 7           | 11       |
| Making a command line utility with gems and thor | 9            | 2           | 6        |
| Will Schenk | Will Schenk                        | 9            | 2           | 5        |
| Making Yosemite Faster | Will Schenk             | 8            | 7           | 8        |
| How to track your coworkers | Will Schenk        | 6            | 1           | 6        |
| Will Schenk - How to track your coworkers        | 3            | 0           | 1        |
+--------------------------------------------------+--------------+-------------+----------+
7 rows in set
```

## Step 6 Adding more commands

Lets create a `Thor` class here for the things that we want to query, and then go through a implement the calls in the `AnalyticsClient` class.

We want to be able to specify the timeframe for when we want the results.  It defaults to the current date, but lets add some more options for **today**, **yesterday**, **recently** (last 7 days), and **month** (last 30 days, which isn't really a month but close enough.)

We also want to have different output options, so we'll add a **table** switch, like the `Hirb` output above, and **csv** to make it easier to plug this into other tools.

We're going to create 4 different ways to query the data.

- What content is getting traffic
- Who is linking to your site
- Who is linking to specific pages on your site
- A timeline of when content was published and people started linking to it

```
Commands:
  ga.rb profiles                     # List Account Profiles
  ga.rb hotcontent PROFILE_ID        # Show hot content for profile id
  ga.rb referers PROFILE_ID          # Show hot content for profile id
  ga.rb content_referers PROFILE_ID  # Show hot content for profile id
  ga.rb timeline PROFILE_ID          # Show a timeline of referers
```

Here's the CLI code:

```rb
class HammerOfTheGods < Thor
  desc "profiles", "List Account Profiles"
  def profiles
    client.print_profiles
  end

  desc "hotcontent PROFILE_ID", "Show hot content for profile id"
  options [:today, :yesterday, :recently, :month, :table, :csv]
  def hotcontent( profile_id )
    result = client.hotcontent( profile_id, start_date, end_date )
    print_result result
  end

  desc "referers PROFILE_ID", "Show hot content for profile id"
  options [:today, :yesterday, :recently, :month, :table, :csv]
  def referers( profile_id )
    result = client.referers( profile_id, start_date, end_date )
    print_result( result )
  end

  desc "content_referers PROFILE_ID", "Show hot content for profile id"
  options [:today, :yesterday, :recently, :month, :table, :csv]
  def content_referers( profile_id )
    result = client.content_referers( profile_id, start_date, end_date )
    print_result( result )
  end

  desc "timeline PROFILE_ID", "Show a timeline of referers"
  def timeline( profile_id )
    client.timeline( profile_id )
  end

  private
  def client
    @client ||= AnalyticsClient.new
  end

  def start_date
    return (Time.now - (24*60*60)).strftime( "%Y-%m-%d" ) if options[:yesterday]
    return (Time.now - (7*24*60*60)).strftime( "%Y-%m-%d" ) if options[:recently]
    return (Time.now - (30*24*60*60)).strftime( "%Y-%m-%d" ) if options[:month]
    nil
  end

  def end_date
    return (Time.now - (24*60*60)).strftime( "%Y-%m-%d" ) if options[:yesterday]
    nil
  end

  def print_result( result )
    if options[:csv]
      client.print_csv_result( result )
    else
      client.print_query_result( result )
    end
  end
end

if __FILE__ == $0
  HammerOfTheGods.start(ARGV)
end
```

## Step 7 Lets implement

We have the `profiles` command and the `hotcontent` command, or **what content is getting traffic** working already.  Lets add some code to make the `--csv` option work, this goes into the `AnalyticsClient` class:

```rb
  def print_csv_result(r)
    csv_string = CSV.generate do |csv|
      csv << r.data.columnHeaders.collect { |x| x.name }
      r.data.rows.each do |row|
        csv << row
      end
    end

    puts csv_string
    csv_string
  end
```

**Who is linking to your site**?
We can find out by looking at `ga:source` which is basically the domain, `ga:referralPath` which is the path part of the url if it's a link referral, and `ga:medium` which will tell you if it's linking from a direct url, social media link, email link, or ad traffic.

```rb
  def referers( profile_id, start_date = nil, end_date = nil )
    query = query_template( profile_id, start_date, end_date )
    query["dimensions"] = "ga:source,ga:referralPath,ga:medium"
    client.execute(
      api_method: api.data.ga.get,
      parameters: query
      )
  end
```

**Who is linking to specific pages on your site** can be dertimined by adding the `ga:landingPagePath` dimension to the above query.  This now breakdown the source of traffic not to the site as a whole, but to a specific landing page.  We're also changing the `sort` query parameter to take this additional dimension into effect.

```rb
  def content_referers( profile_id, start_date = nil, end_date = nil )
    query = query_template( profile_id, start_date, end_date )
    query["dimensions"] = "ga:landingPagePath,ga:source,ga:referralPath,ga:medium"
    query["sort"] = "ga:landingPagePath,-ga:pageviews"

    client.execute(
      api_method: api.data.ga.get,
      parameters: query
      )
  end
  ```

This works, but we can change the way it's printed out to be visually more useful.  In the `HammerOfTheGods` we can flesh it out a bit, so it only prints out your local path once while listing the referals indented so you can scan and see what's going on grouped by page.

```rb
  desc "content_referers PROFILE_ID", "Show hot content for profile id"
  options [:today, :yesterday, :recently, :month, :table, :csv, :full]
  def content_referers( profile_id )
    result = client.content_referers( profile_id, start_date, end_date )
    
    if options[:table] || options[:csv]
      print_result( result )
    else
      last_title = nil

      result.data.rows.each do |row|
        puts "\n#{row[0]}" if last_title != row[0]
        last_title = row[0]
        printf( "     %-5s %-8s %s%s\n", row[4], row[3], row[1], row[2])
      end
    end
  end
```
  
## Step 8 The timeline

**A timeline of when content was published and people started linking to it** can be created by combinding 2 of the methods that we've already written, `hotcontent` and `referers`, and looping through and querying them one day at a time.

We start 30 days ago, and get a list of content for that day.  If we haven't seen it before, we say that it was posted that day.  We then get a list of referrals for that day.  If we haven't seen them before, we print it out.  I'm also supressing links that have passed in less that 2 visitors, since they tend to be very noisey.

```rb
  def timeline( profile_id )
    one_month_ago = Time.now - 30 * 24 * 60 * 60

    start_date = one_month_ago

    today = Time.now

    seen = {}
    title = {}
    while start_date.to_date <= today.to_date
      puts
      puts start_date.to_date

      contents = hotcontent( profile_id, start_date.strftime( "%Y-%m-%d" ), start_date.strftime( "%Y-%m-%d" ) )
      contents.data.rows.each do |content|
        unless title[content[0]]
          puts "  Posted:  #{content[0]}"
        end

        title[content[0]] = true
      end
      puts

      result = referers( profile_id, start_date.strftime( "%Y-%m-%d" ), start_date.strftime( "%Y-%m-%d" ) )
      result.data.rows.each do |data|
        url = data[0]
        url = "http://#{data[0]}#{data[1]}" if data[2] == 'referral'
        printf "  %-10s %5s %s\n", data[2], data[3],url if !seen[url] && data[3].to_i > 2
        seen[url] = true if data[2] == 'referral'
      end
      start_date = start_date + 24 * 60 * 60
    end
  end
```

## Code

The [full code is available to play with](https://gist.github.com/wschenk/d7f8650d619d8f68730a).  The mechanism for talking to Google APIs from a script works everywhere, but if you are going to do this on your server you'll want to get the OAuth2 key using a different process than the `InstalledAppFlow`.
