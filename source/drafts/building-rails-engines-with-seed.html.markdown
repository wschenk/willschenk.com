---
title: Building Rails Engines with Seed
tags: rails, happy_seed, ruby, howto
date: '2015-07-06T21:23:33.987Z'
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

## Create a controller

Inside the main gem/engine directory, lets create a controller.

```bash
$ cd bundled_ui
$ rails g controller bundler
```

Lets add a simple action inside of the newly generated `app/controllers/bundled_ui/bundler_controller.rb`:

```rb
    def index
      @specs = Bundler.definition.specs.to_a
    end
```

And some basic ERB in `app/views/bundled_ui/bundler`:

```erb
<table class="table table-striped table-bordered">
  <tr>
    <tr>
      <th>Gem</th>
      <th>Summary</th>
      <th>Dependancies</th>
    </tr>

    <% @specs.each do |spec| %>
      <tr>
        <td><%= link_to "#{spec.name} #{spec.version}", spec.homepage %></td>
        <td><%= spec.summary %></td>
        <td>
          <ul>
            <% spec.dependencies.each do |dep| %>
              <li><%= "#{dep.name} (#{dep.type})" %></li>
            <% end %>
          </ul>
        </td>
      </tr>
    <% end %>
  </tr>
</table>
```

## Mount the engine

We need to mount the engine in the test server so we can see what's happening.  In `config/routes.rb`:

```rb
BundledUi::Engine.routes.draw do
  root "bundler#index"
end
```

## Start up the test rails server

```bash
$ cd spec/dummy
$ rails s
```

When you go to [http://localhost:3000](http://localhost:3000) you'll see a standard rails install.  This is because we haven't mounted the engines routes.  Lets do that now and reload, in `spec/dummy/config/routes.rb`:

```rb
Rails.application.routes.draw do
  mount BundledUi::Engine => "/bundled_ui"
end
```

Now go to [http://localhost:3000/bundled_ui](http://localhost:3000/bundled_ui).  You should see a list of the Gems inside of your gemspec!

## Layouts

Engines have their own layouts, so lets go in there and add the cdn version of bootstrap.  In `app/views/layouts/bundled_ui/application.html.erb` add the following lines after the `title` tag:

```html
  <script src="//code.jquery.com/jquery-2.1.4.min.js"></script>
  <script src="//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"></script>
  <link href="//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css" rel="stylesheet"/>
```

Now if you reload you should see the bootstrap styled table!

## Lets add some helpers and other awesomeness to the page

Helpers work the same way as always.  Inside of `index.html.erb`, replace the first `<td>` with:

```erb
        <td>
          <%= link_to "#{spec.name} #{spec.version}", spec.homepage %><br/>
          <%= badge_image spec %>
        </td>
```

And create the helper method in `app/helpers/bundler_ui/bundler_helper.rb`:

```rb
    def badge_image( spec )
      image_tag "https://badge.fury.io/rb/#{ spec.name }.svg"
    end
```

## Testing

Lets make sure that the ui only shows itself on local requests.

First lets generate a controller spec:

```sh
$ rails g rspec:controller bundler
```

And write the spec, `spec/controllers/bundled_ui/bundler_controller_spec.rb`:

```rb
require 'rails_helper'

module BundledUi
  RSpec.describe BundlerController, type: :controller do
    # routes { BundledUi::Engine.routes }
    setup do
      @routes = Engine.routes
    end

    it "should return setup information for local requests" do
      p BundledUi::Engine.routes
      get :index, { use_route: :bundled_ui }
      expect(response).to render_template( :index )
    end

    it "should redirect to root url for non-local requests" do
      @request.remote_addr = "1.1.1.1"
      prev = Rails.configuration.consider_all_requests_local
      Rails.configuration.consider_all_requests_local = false
      get :index, { use_route: :bundled_ui }
      expect(response).to redirect_to( root_url )
      Rails.configuration.consider_all_requests_local = prev
    end
  end
end
```

_Note that we need to specify the Engine routes with:_ `{use_route: :bundled_ui}`
