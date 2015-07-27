---
title: Receiving posted JSON with Sinatra
subtitle: small tricks to make things easier
tags: ruby,sinatra,toys
date: '2015-07-27'
---
Here's some simple code to accept a JSON string posted to a [Sinatra]( http://www.sinatrarb.com) end point rather than a form. I switched from using [jQueries](http://jquery.com) `$.ajax` method to [superagent](https://github.com/visionmedia/superagent) as part of my exploration of the node javascript package universe, and it has a different way of serializing nest objects when posting as a form.  Specifically, it doesn't.

I needed something that could do both.

## Code to use form encoding or JSON blob

This first tries and loads the parameters using the normal form encoding methods.  If it doesn't find the `path` parameter, it attempts to parse the body's payload, found in `request.body.read`, using `JSON.parse`:

```rb
  post '/post' do
    payload = params 
    payload = JSON.parse(request.body.read).symbolize_keys unless params[:path]

    logger.info "Saving #{payload[:path]} with #{payload[:meta]}"

    file = load_app.sitemap.find_resource_by_path payload[:path]
  end
```

## Javascript

The coffeescript that calls this is:

```coffeescript
request.post "/api/post"
  .set('Content-Type', 'application/json')
  .send path: @state.path, meta: @state.metadata, body: @state.markdown
  .end (err, response) =>
    if response.ok
      @state.saving = false
      @state.dirty = false
      @trigger @state
```

In this case, `@state.metadata` is also a complex object, and posting it as JSON ensures that it's marshalled correctly.

Not too complicated.
