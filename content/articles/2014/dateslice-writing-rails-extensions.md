---
title: 'Dateslice: Writing rails extensions'
subtitle: "adding date group_by to ActiveRecord"
date: 2014-12-07
tags:
  - rails
  - ruby
  - sql
  - howto
header_image: train.jpg
dark_header: true
aliases:
  - "/dateslice-writing-rails-extensions/"
---

Ruby on Rails is a very modular framework since the merging with Merb in 2008.  The _opinionated conventions_ are implemented under using techniques that let you jump in and build your own components, picking and choosing different parts that let you build Rails apps in the same straightforward way you would if using the official modules.

Let's go through the [`dateslices` gem](https://github.com/HappyFunCorp/dateslices) which I wrote to extend active record so that we could better interact with the `group by` sql command when dealing with dates.  Thanks to [mbrookes](https://github.com/mbrookes) this command now outputs in a format compatible with [Chartkick](http://ankane.github.io/chartkick/) making it a good tool to use when graphing date related things, say user signups, on an admin panel for your application.

## Databases

[ActiveRecord](http://guides.rubyonrails.org/active_record_basics.html) is how Rails interacts with the database, this includes both SQL generation, validations, and a whole bunch more.  It creates a startardized interface over the many subtle differences between SQL implementations on different databases.  Date handling and grouping on dymanic terms is one area where databases differer greatly from one another, and when we want to find counts and sums grouped by different dates we need to tune our SQL for the vagaries of switching to different databases.

## Keeping development the same as production

I tend to develop locally on Sqlite3 and deploy on Postgres.  When we get into something fancy where we want to use some of the amazing features that Postgres has, like [LISTEN/NOTIFY](http://www.postgresql.org/docs/9.2/static/libpq-notify.html) or [hstore](http://postgresguide.com/sexy/hstore.html) , then it makes sense to run a Postgres instance locally.  But most of the time it's overkill, and I prefer running with Sqlite3 since I just need to checkout the project, run bundle, and I'm in a self contained environment.

Pub/sub and attribute store are cool enough things to warrant managing a local Postgres instance, grouping by date doesn't cut it in my book.

If you do already have a Postgres instance running locally, then you should check out the [`groupdate` gem](https://github.com/ankane/groupdate), which is better and worse than `dateslice`: better because it supports Timezones which is awesome and difficult to solve well and worse because it doesn't support Sqlite3.

## Enter dataslice

Lets first take a look at how the SQL differs between the different databases.  The basic structure is

```sql
select aggregation(aggregation_column), timeslice from table group by timeslice
```

Where `aggregation` is one of `count`, `sum`, and `avg`.

`aggregation_column` is the column we are counting, summing or averaging.  For counts, normally we do `count(*)` but we can also do `count(distinct(aggregation_column))` if you only want to count the number of unique occurances.

`timeslice` is the time period that we want to look at.  The basic idea here is that we convert a `datetime` to a string with lower precision (getting rid of the seconds, or minutes, or hours, or days) and then group on that string.  We need to select this on the left side of the query, and we also need it as the input of the `GROUP BY` on the right.

For example, if we want to group by day, this is the SQL that we'd need for the 3 different database variants we are targetting:

<table class="table table-bordered table-sm table-striped">
  <tr><th>Database</th><th>Time Slice</th></tr>
  <tr><th>Mysql</th><td><code>DATE_FORMAT(#{column}, '%Y-%m-%d 00:00:00 UTC')</code></td></tr>
  <tr><th>Sqlite3</th><td><code>strftime( \"%Y-%m-%d 00:00:00 UTC\", #{column} )</code></td></tr>
  <tr><th>Postgres</th><td><code>DATE_TRUNC( 'day' , #{column} )</code></td></tr>
</table>

The different variants can be found in the sourecode for [mysql](https://github.com/HappyFunCorp/dateslices/blob/master/lib/dateslices/mysql.rb), [sqlite](https://github.com/HappyFunCorp/dateslices/blob/master/lib/dateslices/sqlite.rb), and [Postgres](https://github.com/HappyFunCorp/dateslices/blob/master/lib/dateslices/postgresql.rb).  (Notice how Postgres is better here too!)

## Our api

We want to add `group_by_second`, `group_by_minute`, `group_by_hour`, `group_by_day`, `group_by_week`, `group_by_day_of_week`, `group_by_month`, `group_by_year` to ActiveRecord classes that we can use either on the model itself:

```ruby
User.group_by_day
```
Or on a scope:

```ruby
Post.unmoderated.group_by_day
```

And get a resulting hash back like:

```ruby
{
      "2014-07-12 00:00:00 UTC" => 1,
      "2014-07-18 00:00:00 UTC" => 2,
      "2014-07-19 00:00:00 UTC" => 1
}
```

Or, in a `rspec` test, something like this:

```ruby
  it "should return items grouped by day" do
    expect( User.count ).to eq(0)

    @initial_time = Time.parse "2014-07-19 15:26:48 -0400"

    User.create created_at: @initial_time
    User.create created_at: @initial_time - 1.day
    User.create created_at: @initial_time - 1.day
    User.create created_at: @initial_time - 1.week

    expect( User.count ).to eq( 4 )

    res = User.group_by_day( :created_at )

    expect(res).to eq({
      "2014-07-12 00:00:00 UTC" => 1,
      "2014-07-18 00:00:00 UTC" => 2,
      "2014-07-19 00:00:00 UTC" => 1})
  end
  ```

## Building the Rails Extension

Now that we have an idea of what we want to generate, lets take a look at how we build a rails extension.  This is done with the `rails plugin new` command.  We saw the `bundle gem` command before back in the [making a command line utility with gems and thor](/making-a-command-line-utility-with-gems-and-thor/) post, and in many ways they are similar.  But the `rails plugin new` command creates a gem setup for a rails environment for testing and developing your app.

[Happy Seed](http://seed.happyfuncorp.com/) also has a rails plugin generator which will setup rspec testing for you, instead of the default `TestUnit`.  This runs the `rails plugin new` command which sets up the rails gem environment and does a few other things you need to do to get rspec working correctly, and HappySeed will do that stuff for you.

Either way, now you have a new folder with an empty gem that we need to fill out.

We're going to create a `Module`,  `Datelices::Scope`, with our methods and then register our methods with the `ActiveRecord::Base` class.  This looks like so:

 ```ruby
 ActiveRecord::Base.send(:extend, Dateslices::Scopes)
 ```
 This will mixin our methods into all of the classes that extend `ActiveRecord::Base`.

## Metaprogramming with Ruby

Inside of `lib/dateslices.rb` lets define all of the fields that we want to define.

```ruby
module Dateslices
  FIELDS = [:second, :minute, :hour, :day, :week, :day_of_week, :month, :year ]
end
```

Now inside of `lib/dateslices/scopes.rb` we can sketch out our scopes method generator:

```ruby
module Dateslices
  module Scopes
    Dateslices::FIELDS.each do |field|
      define_method :"group_by_#{field}" do |*args|
        # create query based on args, and field
      end
    end
  end
end
```

Lets go through this for a second.  When this code is evaluated, we are going to loop over `Dateslices::FIELDS` and call the `define_method` function for each type of grouping.  These are defined inside of the main `Dateslices` module, and we are naming our method `:"group_by_#{field}"`.  Image that a developer writes `User.group_by_day( :updated_at )`, what happens then?

When that is invoked the Ruby runtime is actually invoking the closure that we are passing into `define_method`, which is generated inside of the loop with a different value for `field` on each one.  In addition to using this inside of the name of the function, this value is available inside of the body.  The `*args` on the other hand, and in our example it would equal `[:updated_at]`, comes from the method invocation as we would expect.

We are writing code which generates code, and some of the variables are part of our desire "not to write 15 of the basically the same methods" and some of the variables are there to tweak the functionality of the API.

## The SQL Bit

The full details can be found in [the repo](https://github.com/HappyFunCorp/dateslices/blob/master/lib/dateslices/scopes.rb) but here's the bit that actually generates the query switching out to the various classes that know how to deal with each of the databases that we saw before.

```ruby
args = args.dup

column = args[0].blank? ? 'created_at' : args[0]
aggregation = args[1].blank? ? 'count' : args[1]
aggregation_column = args[2].blank? ? '*' : args[2]

sql = ["#{aggregation}(#{aggregation_column}) as count"]

time_filter = case connection.adapter_name
                when 'SQLite'
                  Dateslices::Sqlite.time_filter(column, field)
                when 'PostgreSQL', 'PostGIS'
                  Dateslices::Postgresql.time_filter(column, field)
                when 'MySQL', 'Mysql2'
                  Dateslices::Mysql.time_filter(column, field)
                else
                  throw "Unknown database adaptor #{connection.adapter_name}"
              end

sql << "#{time_filter} as date_slice"

slices = select( sql.join(', ')).where.not(column => nil).group('date_slice').order('date_slice')
```

The full code does a bit more to figure out how you want to see the data, but that's the tricky stuff.

## Testing a rails plugin that talks to many databases

The first thing that's a little strange when testing a plugin is that you can test your gem in two different contexts: one in a basic ruby context, and your tests go into `spec/`, and the other in a rails context.  The plugin generator will create a sample rails app inside of `spec/dummy` (or `test/dummy` if you are using `rails plugin new` without our fancy rspec stuff).  

Let's take a look now at how to test a gem that talks to many different databases.  Normally when we start up a rails environment, test or otherwise, it connects to a database and that's that.  However, we need to run the same test suite over 3 different databases making sure that the gem behaves in exactly the same way for each one.

Here's our `spec/dummy/spec/models/test_spec.rb`

```ruby
require 'rails_helper'
require 'dateslice_tester'
require 'groupdate_tester'

databases = [{ :adapter => 'mysql', :database => 'dateslice_test', :user => 'root'},
             { :adapter => 'postgresql', :database => 'dateslice_test'},
             { :adapter => 'sqlite3', :database => 'db/test.sqlite3'}]

formats = ['groupdate', 'dateslice']

databases.each do |database|
  formats.each do |format|
    RSpec.describe "#{database[:adapter].titleize} #{format}", :type => :model do
      include_examples format, database
    end
  end
end
```

_This code was based on something I cobbled together but cleaned up by [mbrookes](https://github.com/mbrookes), thanks mbrooks!_

We first define a set a database configuration for our test databases.  We then loop over that, and use the `include_examples` feature of `RSpec`, passing in the both the output format and the database configuration that we want to test.  We have two files of examples, one which defines the `groupdate` format, and the other which defines the `dateslices` format.  Once again I'd like to point out that if you don't care about SQLite3 support and want Timezone support, the [groupdate](https://github.com/ankane/groupdate) is what you want.

Lets look at the opening stanzas of `spec/dummy/spec/dateslice_tester.rb`

```ruby
RSpec.shared_examples "dateslice" do |config|
  before :context do
    puts "Setting up #{config}"

    ActiveRecord::Base.establish_connection config

    puts "Trying to migrate"
    ActiveRecord::Migration.create_table :users, :force => true do |t|
      t.string :name
      t.integer :score
      t.timestamp :created_at
      t.timestamp :updated_at
    end
  end

  before do
    Dateslices.output_format = :dateslice
    User.delete_all
  end

  it "should return items grouped by day"
end
```

`RSpec.shared_examples` is the counter part to the `include` examples above, and when it gets called the database `config` is passed in.  We then call  `ActiveRecord::Base.establish_connection config` to connect `ActiveRecord` to the database as part of the `before :context` part of the RSpec life cycle.

Next we need to actually create the database tables that we are going to run tests over.  Since we are switching the databases as part of the testing process itself, it makes no sense to use `rake db:create:test` to create the DDL, since which database would that be creating?  We need to do 3 different ones, and we certainly don't want to have an elaborate process to start any of the tests if you decide to add an additional migration.  So we call a migration directly from the code, turning `:force => true` so even if it already exists we push the current definition there.

```ruby
ActiveRecord::Migration.create_table :users, :force => true
```

And then in the regular `before` callback we make sure that the tables are cleared our ready for the next test.

## Just a bit of ActiveRecord

We've just gone through an ActiveRecord extention and that barely scratches the surface of what else you can do with Rails.  [Crafting Rails 4 Applications](https://pragprog.com/book/jvrails2/crafting-rails-4-applications) is the best resource I've found to get a sense of what is possible, but when I sat down to work on something there was a lot of trial and error.  They through how they created [mail_form](https://github.com/plataformatec/mail_form), or at least a simplified version of it, that lets you use rails validations from ActiveRecord without having to back up the model with a database.  (As you might infer from the name, something that is useful for Contact forms that send out email.)  The book also goes through how Rails Engines work, which are very much like rails plugins but with additional integration points into the rails application lifecycle.
