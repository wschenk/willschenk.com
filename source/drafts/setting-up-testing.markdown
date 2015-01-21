---
title: 'Setting up Rails testing with rspec, devise, and the gang'
subtitle: 'so much fun'
# date: TBD When publishing
tags: rails, happy_seed, testing
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

## Configure rspec

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
