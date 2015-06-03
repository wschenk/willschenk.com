---
title: 'Building Rails Engines with Seed'
# date: TBD When publishing
tags: rails, happy_seed, testing, ruby, howto
---

[Rails Engines](http://guides.rubyonrails.org/engines.html) are a type of rails plugin, which allows you to package up functionality inside of a gem that can be shared across projects.  An engine is a _mountable_ plugin, which means that you can add routes, controllers, actions and views.  

Lets walk through building a simple rails engine that shows you information about the gems that you have installed on your application.  These pages are meant to be available only on development, but this is just business logic.


## Create the project

```bash
$ happy_seed engine bundled_ui
```

This creates a new rails plugin with the `--mountable` option and sets it up for testing using rspec.  The base command is `rails plugin new bundled_ui -T --dummy-path=spec/dummy --mountable` for those that want to do it by hand.

The directory that is generated is has a few different things than a rails plugin.

- `app` is where your controllers, models, and views go
- `bundled_ui.gemspec` is where you specify runtime and development dependancies
- `config/routes.rb` is where you specify the routes that the engine provides
- `spec/dummy` is where a test application goes, so you can develop your app.

Let's get started.

## 