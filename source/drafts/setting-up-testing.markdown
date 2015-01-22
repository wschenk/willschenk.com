---
title: 'Setting up Rails testing with rspec, devise, and the gang'
subtitle: 'so much fun'
# date: TBD When publishing
tags: rails, happy_seed, testing, ruby
---

The goal is to get features out fast, and interate on them quickly.  Does anyone care about it?  What do they care about?  How do we make it better?

As projects get bigger, both in terms of people using the site as well as people _working_ on the site, testing and quality become relatively more important.  Adding tests introduces drag, and the theory is that you invest now for payoffs later.  What's the least amount of drag we can add to the process to get us in a good place for when it will start to pay off?

RSpec feature specs lets developers create testing click paths through the application, looking at the flow though the app.  It doesn't focus enough on the design details, and going through a test driving development process with this way will end up with sites 'working' using only an engineer's defination, so as a MVP implementor you'll need to find a way to resist the temptation, no matter how nifty the backend flow state is.

This article will go through how the testing environment is set up on [HappySeed](http://seed.happyfuncorp.com) where we focus mainly on RSpec feature specs.  Seed does this all automatically, but its nice to have it written down in a one place so we all know what's going on.  Let's get into it.

## Install RSpec, FactoryGirl, Capybara, and Guard

Lets fire up a new rails app, and pass the `-T` option to not include TestUnit.

```sh
$ rails new test_app -T
```

Now add some gems to the gem file.

```rb
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

```sh
$ spring binstub --all
* bin/rake: spring already present
* bin/rspec: generated with spring
* bin/rails: spring already present
```

This creates a `bin/rspec` command that lets us run our tests.  Lets create a Guardfile and then tell guard to use this command:

```sh
$ guard init
```

Then edit the newly generated `Guardfile` to use the spring version of the rspec runner.  I also like to run all of the tests when I start up guard, because the more the merrier.  In `Guardfile`, change the `guard :rspec` line to:

```rb
guard :rspec, cmd: "bin/rspec", all_on_start: true do
```

Now we install the rspec files:

```sh
$ rails g rspec:install
```

This creates 3 files:

```sh
create  .rspec
create  spec
create  spec/spec_helper.rb
create  spec/rails_helper.rb
```

I like to have the output in documentation format by default, so we can edit the `.rspec` file to be:

```rb
--color
--require spec_helper
--format documentation
```

## Add login support and factory girl to spec_helper

Getting feature and controller specs to work with authentication is a bit tricky.  We're going to assume that you use [devise for authentication](/setting-up-devise-with-twitter-and-facebook-and-other-omniauth-schemes-without-email-addresses/), but this should support anything that uses [warden](https://github.com/hassox/warden):

Create `spec/support/controller_helpers.rb`

```rb
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

```rb
require_relative 'support/controller_helpers'
require 'devise'
```

and in the config block add:

```rb
  config.include ControllerHelpers, type: :controller
  Warden.test_mode!

  config.after do
    Warden.test_reset!
  end
```

Then edit `rails_helper.rb` and put the following in the config blog:

```rb
  config.include FactoryGirl::Syntax::Methods
  config.include Devise::TestHelpers, type: :controller
  config.include Warden::Test::Helpers
  end
```

**If you are getting uncaught throw :warden errors then make sure you have the right includes in the right config files!**

This gives you a few things:

- `FactoryGirl` syntax methods, which let you use things like `create( :user )` in your tests, which uses `FactoryGirl` to generate a database object.
- helper methods to `sign_in` a user for a specific scope.  This could be used as `sign_in create( :user )` at the beginning of your controller test methods, and `sign_in nil` to simulate an anonymous user.

## Example controller spec

Assuming that you've already setup devise, lets look at how to implement a controller spec.  First lets create a simple scaffold for a post:

```sh
$ rails g scaffold post name:string
```

And lets make it require authentication in `posts_controller.rb`:

```rb
class PostsController < ApplicationController
  before_action :authenticate_user!
```

And replace `posts_controller_spec.rb` with:

```rb
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

When we run `guard`, it should try and run this tests but fail because we can't configured a user factory.

Lets set that up now in `spec/factories/user.rb`:

```rb
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

Going back to the guard window, we should see our tests successfull.  Press _return_ to run everything again and verify.  Lets look at how to simulate a logged in user:

```rb
  it "should let a user see all the posts" do
    login_with create( :user )
    get :index
    expect( response ).to render_template( :index )
  end
```

## Example rspec feature

Feature specs are more interesting, since they let you walk through the site and how it works.  Lets build one now:

```sh
$ rails g rspec:feature add_new_post
      create  spec/features/add_new_posts_spec.rb
```

And lets add something there now:

```rb
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

```rb
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

At the top of `rails_helper.rb`:

```rb
require 'webmock/rspec'
```

And VCR lets you record and play back tests in the test environment, so the first time it's called it lets it get through, and then on subsequent requests it plays back a known response.  Put this at the bottom of `rails_helper.rb` to configure:

```rb
VCR.configure do |c|
  c.cassette_library_dir  = Rails.root.join("spec", "vcr")
  c.hook_into :webmock
end
```