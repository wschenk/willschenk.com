---
title: Setting up Rails testing with rspec, devise, and the gang
subtitle: so much fun
tags:
  - rails
  - happy_seed
  - testing
  - ruby
date: 2015-01-23
historical: true
aliases:
  - "/setting-up-testing/"
---
The goal is to get features out fast, and interate on them quickly.  Does anyone care about it?  What do they care about?  How do we make it better?

As projects get bigger, both in terms of people using the site as well as people _working_ on the site, testing and quality become relatively more important.  Adding tests introduces drag, and the theory is that you invest now for payoffs later.  What's the least amount of drag we can add to the process to get us in a good place for when it will start to pay off?

RSpec feature specs lets developers create testing click paths through the application, looking at the flow though the app.  It doesn't focus enough on the design details, and going through a test driving development process with this way will end up with sites 'working' using only an engineer's defination, so as a MVP implementor you'll need to find a way to resist the temptation, no matter how nifty the backend flow state is.

This article will go through how the testing environment is set up on [HappySeed](http://seed.happyfuncorp.com) where we focus mainly on RSpec feature specs.  Seed does this all automatically, but its nice to have it written down in a one place so we all know what's going on.  Let's get into it.

## Install RSpec, FactoryGirl, Capybara, and Guard

Lets fire up a new rails app, and pass the `-T` option to not include TestUnit.

```bash
$ rails new test_app -T
```

Now add some gems to the `Gemfile`.

```ruby
group :development, :test do
  gem 'rspec-rails'
  gem 'factory_girl_rails'
  gem 'capybara'
  gem 'guard-rspec'
  gem 'spring-commands-rspec'
  gem 'vcr'
end

group :test do
  gem 'webmock'
end
```

Lets go through this:

- [`rpsec`](http://rspec.info) (implied as a dependancy) is the testing framework that we are going to use
- [`factory_girl`](https://github.com/thoughtbot/factory_girl) (implied) lets us code smart database fixtures in ruby, in things called _factories_
- [`capybara`](https://github.com/jnicklas/capybara) lets helps you test web applications by simulating how a real user would interact with your app.
- [`guard`](https://github.com/guard/guard) (implied) makes rerunning tests a snap, by watching the filesystem for when you save files and triggering events automatically
- [`webmock`](https://github.com/bblimke/webmock) locks down your test environment from talking to the internet
- [`vcr`](https://github.com/vcr/vcr) record your test suite's HTTP interactions and replay them during future test runs for fast, deterministic, accurate tests
- [`rspec-rails`](https://github.com/rspec/rspec-rails) is the rails integration with rspec, which depends upon rspec itself.
- [`factory_girl_rails`](https://github.com/thoughtbot/factory_girl_rails) is the rails integration with factory_girl.
- [`guard-rspec`](https://github.com/guard/guard-rspec) is rspec integration with guard
- [`spring-commands-rspec`](https://github.com/jonleighton/spring-commands-rspec) lets you integrate rspec with spring, which means that your tests will run much faster.

If you are interested in using cucumber, see below for steps to integrate it.

## Base configuration of rspec and guard

Lets make sure that spring knows about rspec:

```bash
$ bundle install
$ spring binstub --all
* bin/rake: spring already present
* bin/rspec: generated with spring
* bin/rails: spring already present
```

This creates a `bin/rspec` command that lets us run our tests.  Lets create a Guardfile and then tell guard to use this command:

```bash
$ guard init
```

Then edit the newly generated `Guardfile` to use the spring version of the rspec runner.  I also like to run all of the tests when I start up guard, because the more the merrier.  In `Guardfile`, change the `guard :rspec` line to:

```ruby
guard :rspec, cmd: "bin/rspec", all_on_start: true do
```

Now we install the rspec files:

```bash
$ rails g rspec:install
```

This creates 3 files:

```bash
create  .rspec
create  spec
create  spec/spec_helper.rb
create  spec/rails_helper.rb
```

I like to have the output in documentation format by default, so we can edit the `.rspec` file to be:

```ruby
--color
--require spec_helper
--format documentation
```

## Add login support and factory girl to spec_helper

Getting feature and controller specs to work with authentication is a bit tricky.  We're going to assume that you use [devise for authentication](/setting-up-devise-with-twitter-and-facebook-and-other-omniauth-schemes-without-email-addresses/), but this should support anything that uses [warden](https://github.com/hassox/warden):

Add devise to `Gemfile`:

```ruby
gem 'devise'
```

Create `spec/support/controller_helpers.rb`

```ruby
module ControllerHelpers
  def login_with(user = double('user'), scope = :user)
    current_user = "current_#{scope}".to_sym
    if user.nil?
      allow(request.env['warden']).to receive(:authenticate!).and_throw(:warden, {:scope => scope})
      allow(controller).to receive(current_user).and_return(nil)
    else
      allow(request.env['warden']).to receive(:authenticate!).and_return(user)
      allow(controller).to receive(current_user).and_return(user)
    end
  end
end
```

At the top of `spec_helper.rb` add:

```ruby
require_relative 'support/controller_helpers'
require 'devise'
```

and in the config block add:

```ruby
  config.include ControllerHelpers, type: :controller
  Warden.test_mode!

  config.after do
    Warden.test_reset!
  end
```

Then edit `rails_helper.rb` and put the following in the config block:

```ruby
  config.include FactoryGirl::Syntax::Methods
  config.include Devise::TestHelpers, type: :controller
  config.include Warden::Test::Helpers
```

**If you are getting uncaught throw :warden errors then make sure you have the right includes in the right config files!**

This gives you a few things:

- `FactoryGirl` syntax methods, which let you use things like `create( :user )` in your tests, which uses `FactoryGirl` to generate a database object.
- helper methods to `sign_in` a user for a specific scope.  This could be used as `sign_in create( :user )` at the beginning of your controller test methods, and `sign_in nil` to simulate an anonymous user.

## Example controller spec

This example assumes that you've already setup devise.  If not, you can get something up and running doing:

```
$ rails g devise:install && rails g devise User && rake db:migrate
```

(I go into slightly more detail in setting up [devise for authentication](/setting-up-devise-with-twitter-and-facebook-and-other-omniauth-schemes-without-email-addresses/).)

Assuming that you've already setup devise, lets look at how to implement a controller spec.  First lets create a simple scaffold for a post:

```bash
$ rails g scaffold post name:string
```

And lets make it require authentication in `posts_controller.rb`:

```ruby
class PostsController < ApplicationController
  before_action :authenticate_user!
```

And replace `posts_controller_spec.rb` with:

```ruby
require 'rails_helper'

RSpec.describe PostsController, :type => :controller do
  describe "anonymous user" do
    before :each do
      # This simulates an anonymous user
      login_with nil
    end

    it "should be redirected to signin" do
      get :index
      expect( response ).to redirect_to( new_user_session_path )
    end
  end
end
```

When we run `guard`, a few of the scaffold generate things will fail, since we are now forcing signin to check out this page.  Our spec will correctly redirect the user to sign in.

Lets now simulate a logged in user:

```ruby
  it "should let a user see all the posts" do
    login_with create( :user )
    get :index
    expect( response ).to render_template( :index )
  end
```

Guard should try and run this tests but fail because we can't configured a user factory.

Lets set that up now in `spec/factories/user.rb`:

```ruby
FactoryGirl.define do
  sequence :email do |n|
    "person#{n}@example.com"
  end
end

FactoryGirl.define do
  factory :user, :class => 'User' do
    email
    password '12345678'
    password_confirmation '12345678'
  end
end
```

Going back to the guard window, press _return_ to run everything again and verify.  (Guard doens't know that the `posts_controller_spec.rb` depends upon the user factory changing.)


## Example rspec feature

Feature specs are more interesting, since they let you walk through the site and how it works.  Lets build one now:

```bash
$ rails g rspec:feature add_new_post
      create  spec/features/add_new_posts_spec.rb
```

And lets add something there now:

```ruby
require 'rails_helper'

feature "AddNewPosts", :type => :feature do
  it "should require the user log in before adding a post" do
    password = "123456789"
    u = create( :user, password: password, password_confirmation: password )

    visit new_post_path

    within "#new_user" do
      fill_in "user_email", with: u.email
      fill_in "user_password", with: password
    end

    click_button "Log in"

    within "#new_post" do
      fill_in "post_name", with: "Post title"
    end

    click_link_or_button "Create Post"

    expect( Post.count ).to eq(1)
    expect( Post.first.name).to eq( "Post title")
  end
end
```

This first creates a new user in the database, and then

1. Visits the `new_post_path` url.  
2. We should be shown the login form, and we fill it out
3. Click the log in button
4. Fill out the new post form
5. Create the post
6. Make sure that it's in the database

If you want to skip the steps of manually logging in the user, you can use the `login_as` method like so:

```ruby
  it "should create a new post with a logged in user" do
    login_as create( :user ), scope: :user

    visit new_post_path
    # puts page.body

    within "#new_post" do
      fill_in "post_name", with: "Post title"
    end

    click_link_or_button "Create Post"

    expect( Post.count ).to eq(1)
    expect( Post.first.name).to eq( "Post title")

  end
```

## Setting up webmock and VCR

Webmock stops requests from hitting the network in the test environment.  Lets turn this on by editing `rails_helper.rb`:

In `rails_helper.rb`, below `require 'rails/rspec'`:


```ruby
require 'webmock/rspec'
```

And VCR lets you record and play back tests in the test environment, so the first time it's called it lets it get through, and then on subsequent requests it plays back a known response.  Put this at the bottom of `rails_helper.rb` to configure:

```ruby
VCR.configure do |c|
  c.cassette_library_dir  = Rails.root.join("spec", "vcr")
  c.hook_into :webmock
end
```

## Example usage of webmock and VCR

Word comes down from up high that to facilitate decision making, a key bit of information is the current phase of the moon when something is posted to the site.  We can pull the data from the [Cerridwen](http://cerridwen.readthedocs.org/en/latest/index.html) site, but how do we set up tests for this?  We certainly don't want our test suite to fail because the phase of the moon changed!

This is where webmock and VCR comes in.  Webmock by itself will throw an error when a request is made over the network.  VCR will let us record and play back network request -- using _cassettes_ no less -- so that we isolate changing API responses from our tests.

First lets write our code to look up the phase of the moon on the create post action.  Let's add a field

```ruby
$ rails g migration add_phase_of_moon_to_posts moon_phase:string && rake db:migrate
```

Then add the lookup to our `app/controllers/posts_controller.rb` and replace the `def create` method:

```ruby
  require 'json'
  require 'net/http'
  def create
    @post = Post.new(post_params)

    moon_json = Net::HTTP.get( URI.parse("http://cerridwen.viridian-project.de/api/v1/moon" ))
    moon = JSON.parse( moon_json ).first

    @post.moon_phase = "#{moon['phase']['trend']} in #{moon['position']['sign']}"

    @post.save
    respond_with(@post)
  end
```

(This is not good style.)

Now lets run our test again.

Lots of unhandled http request errors!  I'm going to ignore the scaffold generate tests and focus on our rspec feature.  So, inside of `spec/features/add_new_posts_spec.rb` we can link the click action inside of a use_cassette block like so::

```ruby
  it "should create a new post with a logged in user" do
    login_as create( :user ), scope: :user

    visit new_post_path
    # puts page.body

    within "#new_post" do
      fill_in "post_name", with: "Post title"
    end

    VCR.use_cassette "waxing_in_pisces" do
      click_link_or_button "Create Post"

      expect( Post.count ).to eq(1)
      expect( Post.first.name).to eq( "Post title")
      expect( Post.first.moon_phase).to eq( "waxing in Pisces" )
    end
  end
```

And let the tests run.  When tests are run, VCR is going to look for the file `spec/vcr/waxing_in_pisces`.  If it's not found, then it will make the request to the server.  If the API requires authentication, for example an accesstoken when accessing a OAuth protected call, then you'll need to configure you tests to have valid credentials for the first time.  These credentials could be written into the spec/vcr directory, so make sure that you scrub that before things get checked in.  (Or expire the access tokens.)

If it is found, then it plays it back and so the next time the suite is run it plays it back.  Which is way faster.

## Final notes

Testing has a place in software development, but it's not actually clear where that is.  Setting up test haresses and building out tests for every feature really slows things down in the beginning, and probably isn't appropriate for one off, exploratory things.  On the other hand, things tend to grow way past what anyone ever initially expected, and you always say "I wish there was a test suite" when inheriting code -- or even more likely, looking at something you haven't seen for a few months.

The rails community has a [long history with Test Driven Development](http://martinfowler.com/articles/is-tdd-dead/).  The biggest advantage of this is that you write code that is testable, and code that is testable has all sorts of other qualities that is great, from clearly defined boundries of responsibility, good encapulation, and being able to look at small isolated pieces of the system and having it make sense.  On the other hand, thebest code is the stuff you don't need to maintain, and in some projects the weight of the test code is as large or larger than the code for the application features.

The biggest problem with testing is that it gets you in a wonderful, but wrong, flow state of development.  In the begining of projects you should be focused on the user experience, and how the system is experienced from the outside.  The highest level tests tend to be are user stories, which is below the level of brand and user experience.
