---
title: "Building a hugo site and theme with Bootstrap"
subtitle: "hugo is blazing fast"
date: 2018-10-21
tags:
  - bootstrap
  - static_sites
  - tools
  - howto
---

Now that's its 2018 its time to retool the blog using hugo.  Why not?  Hugo is built in golang and is blazing fast and everything is cleaner than it was in the middleman years.

One of the reasons that I liked middleman -- it's usage of the rails' Sprockets library -- is no longer a strength.  The javascript and front-end world has moved over to WebPack and I've personally moved over to create-react-app.  We'll get into that later.  First go and [download hugo from the main site](https://gohugo.io/getting-started/installing/). And then we'll create a blank site and theme.

```bash
$ hugo new site mysite
$ cd mysite
$ hugo new theme mytheme
```

Edit the generated `config.toml` to have it point to this new theme.

```toml
theme = "mytheme"
relativeURLs = true
```

I like to setup all the content sites so the urls are relative so I can host in subdirectories and the like.  (I'll talk about this at a later post, but this makes it easy to stick things in ipfs and load up development builds of the site on an actual mobile device for testing.)  Finally, lets start the server:

```bash
$ hugo server --watch --verbose --buildDrafts --cleanDestinationDir --disableFastRender
```

These are some useful development flags, `---buildDrafts` does what you'd expect and `--disableFastRender` helps make sure that your shift-reloading gets the latest version of the fine.

## Layouts, blocks and partials

First we'll start with the base layout in `themes/mytheme/layouts/_default/baseof.html`

```go-html-template
<!DOCTYPE html>
<html>
  {{- partial "head.html" . -}}
  <body>
    {{- partial "header.html" . -}}
    {{- block "main" . }}{{- end }}
    {{- partial "footer.html" . -}}
  </body>
</html>
```

This is the `_default` base layout for everything in the site.  You can have specific layouts for different content types, but this is the fallback one that we are going to define for when nothing else is specified.  Inside of this we define a few `partial`s and `block`s.  You can think of partials as included subtemplates that we can use both for structure as well as a convenient place to override the structure of a theme when you want to do big customizations.  `blocks` are replaced with the data from the content of your site, so are better thought of as the _parts where the template is filled with data_.

The `{{ }}` stuff is the go templating language, so if you want to really figure out how that works you'll eventually find yourself in the golang language docs, which is a little intense sometimes.  The main thing to remember is that for every page that is rendered on the site its passed in a `HugoPage` object which is where you need to pull out all of the data.  So if you want to know what is available to you, that's where you start looking.

### Head

Inside `themes/mytheme/layouts/partials/head.html` add bootstrap and the various bits of awesomeness required for that.

```go-html-template
<head>
  <title>{{ .Title }}</title>
  <!-- Required meta tags -->
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">

  <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">
  <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" crossorigin="anonymous"></script>
  <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" integrity="sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy" crossorigin="anonymous"></script>

  {{ range .AlternativeOutputFormats -}}
    {{ printf `<link rel="%s" type="%s+%s" href="%s" title="%s" />` .Rel .MediaType.Type .MediaType.Suffix .Permalink $.Site.Title | safeHTML }}
  {{ end -}}

</head>
```

This sets up bootstrap to lead from the CDN, as well as adding a link to the generated rss feed.  Here we can see a few other things.  One is that we are setting the `<title>` using `{{ .Title }}`.  We may want to change that later, but what that means is that we are using the current page's title attribute to set the title, and if you wanted to include something from the `.Site` data this is where you'd put the logic.

The other thing here is the `range` function.  In practical terms we are putting in `rss` discovery links, but what this literally is going is looping over the `AlternativeOutputFormats` slice inside of the page.  `range` is the golang looping idiom, which is like a `for` loop but better.

### Footers

Lets put some of these variables into the footer to help debug a bit.  This is in `themes/mytheme/layouts/partials/footer.html` where we are going to expose to hugo page variables so we know what's happening

```go-html-template
<footer class="footer mt-5">
  <div class="container">
    <table class="table table-sm">
      <caption>Hugo Variables for current page</caption>
      <tr><th>Name</th><td>{{ .Name }}</td></tr>
      <tr><th>Kind</th><td>{{ .Kind }}</td></tr>
      <tr><th>Type</th><td>{{ .Type }}</td></tr>
      <tr><th>List Page</th><td>.Pages</td></tr>
      <tr><th>IsPage</th><td>{{ .IsPage }}</td></tr>
      <tr><th>IsHome</th><td>{{ .IsPage }}</td></tr>
      <tr><th>Next</th><td>{{ .Next }}</td></tr>
      <tr><th>Prev</th><td>{{ .Prev }}</td></tr>
      <tr><th>Section</th><td>{{ .CurrentSection }}</td></tr>
    </table>
  </div>
</footer>
```

### Page templates

Now we will define a basic single template, which will be used to render a single object, not a collection of anything.  We are going to demo this with the index page initially.
`themes/mytheme/layouts/_default/single.html` :

```go-html-template
{{ define "main" }}
<div class="container">
  <p>This from the single page template</p>
</div>
{{ end }}
```

And out first draft of the `index.html` template for the site.  We'll put in a little little jumbotron in `themes/mytheme/layouts/index.html`:

```go-html-template
{{ define "main" }}
<div class="jumbotron">
  <div class="container">
    <h1 class="display-4">This could be the title</h1>
    <p class="lead">Here's some description stuff</p>
    <p>There's also other things that are super nice</p>
  </div>
</div>
{{ end }}
```

At this point you should be able to see the index page when you go to http://localhost:1313 (If hugo didn't create an empty header.html partial the build might fail, so move on the next section if you don't have that.)

## Header and menus

In `config.toml` on the top level of your site (out of the theme) lets define a few menu items.  We will then use that data to fill in the header.

```toml
[menu]
  [[menu.main]]
    identifier = "about"
    name = "About"
    url = "/about/"

  [[menu.main]]
    identifier = "posts"
    name = "Posts"
    url = "/posts/"
```

Then add a header in `themes/mytheme/partials/header.html`.  We are going to use a bootstrap navbar and pull the menu items out of the site configuration.

```go-html-template
<nav class="navbar navbar-expand-lg navbar-light ">
  <a class="navbar-brand" href="{{ "/" | relURL}}">{{.Site.Title}}</a>
  <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
    <span class="navbar-toggler-icon"></span>
  </button>

  <div class="collapse navbar-collapse" id="navbarSupportedContent">
    <ul class="navbar-nav ml-auto">
      {{ $currentPage := . }}
      {{ range .Site.Menus.main }}
        <li class="navbar-item {{if or ($currentPage.IsMenuCurrent "main" .) ($currentPage.HasMenuCurrent "main" .) }} active{{end}}">
          <a class="nav-link" href="{{ .URL }}" title="{{ .Title }}">{{ .Name }}</a>
        </li>
      {{ end }}
    </ul>
  </div>
</nav>
```

There are a couple of new concepts here, the first is `{{ "/" | relURL }}` which is a helper function to transform a variable in somewhat that could make it more palatable to the user.  In this case I'm just translating "/" to a relativeURL, which on subpages will have it be something like "../../index.html" or whatever.

`{{ $currentPage := . }}` is setting a variable so let us refer to the outcontext when we are in the `range` below.  The `range` links over all of the menu items.  The object that `.` refers to changes inside of the `range` to be the current item that it's iterating over, so in order to make any comparisons to the currentPage in this case we need to give it a name.  `:=` is very golang.

Also are `if` statements, which we are using to determine if we include the `active` class on the link.  Note where the operator is, that is the `or` comes first and the data comes afterwards.  So lispy!

## Adding post content

Lets now add some basic posts.

```
$ hugo new posts/sample_post.md
```

If you didn't start hugo with -D, you'll need to make sure that the draft flag isn't true for it to show up

```
---
title: "Sample Post"
date: 2018-10-19T16:04:51-04:00
draft: false
---

One of the things I'm very interested in is writing words and seeing them on the page.
```

This is pretty standard from more static site generators with the front matter on the top and the markdown on the bottom.

## List templates

Create a list template view in `themes/mytheme/layouts/_default/list.html`

```go-html-template
{{ define "main" }}
<div class="container">
  <h1 class="text-center">
    {{ if eq .Kind "taxonomy" }}
      {{ .Name | humanize }}
    {{ else }}
      {{ .Type | humanize }}
    {{ end }}
  </h1>
  {{ range .Pages }}
    <div class="row mt-2">
      <div class="d-none d-sm-block col-sm-2 mt-auto offset-sm-2 text-right">
        <time class="post-date" datetime="{{ .Date.Format "2006-01-02T15:04:05Z07:00" | safeHTML }}">{{ .Date.Format "Jan 2, 2006" }}</time>
      </div>
      <div class="col">
        <a class="text-body" href="{{ .URL | relURL }}">{{ if .Draft }}DRAFT: {{end}}{{ .Title | markdownify }}</a>
      </div>
    </div>
  {{ end }}
</div>
{{ end }}
```


We're using a hugo function `humanize` to capitalize the type of object that we are looking at, and a whole bunch of bootstrap utility classes to align and only show the published date on larger screens.  There's an `if` statement that either displays the name of the taxonomy -- which is the `Kind` of tag pages -- or the object type for list pages.

The `.Date.Format` format string is really weird -- I'm not sure that I really understand it but it expects the year to be 2006 for things to make sense.  This is part of the way that the hugo's underlying go date formating stuff works that.

Now we click on posts in the nav bar you should see the list of posts.  But when we click on the page itself, you'll notice that we have the text "This from the single page template".  So lets update that `themes/mytheme/layouts/_default/single.html` template now:

```go-html-template
{{ define "main" }}
<div class="container">
  <h1>{{ .Title | markdownify}}</h1>

  {{ .Content }}
</div>
{{ end }}
```

## Adding tags

Since we mentioned tags above, lets go right ahead and add that taxonomy to our theme.  Another common type of taxonomy is categories.  Let's go ahead and add that to the `theme.toml`:

```toml
tags = ["tags"]
```

Restart your hugo server to see the magic!


Lets add some tags to our first `sample_post.md` file

```
---
title: "Sample Post"
date: 2018-10-19T16:10:36-04:00
draft: false
tags: [ "one", "two" ]
---

This is the first paragraph of what I'd like to say.
```

And then create another post

```bash
hugo new posts/sample_post_the_second.md
```

```
---
title: "Sample Post The Second"
date: 2018-10-19T16:54:29-04:00
draft: false
tags: ["two"]
---

This is the second amazing post that will *blow your mind*!
```

New lets add tags to our menu so we can put it on the nav bar.  We'll also specify menu weights to fix the order.  This is done in the site configuration `config.toml` where we are configuring how this specific site used the theme that we are defining.

```toml
[menu]
  [[menu.main]]
    identifier = "about"
    name = "About"
    url = "/about/"
    weight = 100

  [[menu.main]]
    identifier = "tags"
    name = "Tags"
    url = "/tags/"
    weight = 110

  [[menu.main]]
    identifier = "posts"
    name = "Posts"
    url = "/posts/"
    weight = 120
```

Finally, lets add some logic to our theme's `theme/mytheme/partials/header.html`.  You can replace the whole thing

```go-html-template
<nav class="navbar navbar-expand-lg navbar-light ">
  <a class="navbar-brand" href="{{ "/" | relURL}}">{{.Site.Title}}</a>
  <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
    <span class="navbar-toggler-icon"></span>
  </button>

  <div class="collapse navbar-collapse" id="navbarSupportedContent">
    <ul class="navbar-nav ml-auto">
      {{ $currentPage := . }}
      {{ range .Site.Menus.main }}
        {{ if not (eq .Identifier "tags") }}
          <li class="navbar-item {{if or ($currentPage.IsMenuCurrent "main" .) ($currentPage.HasMenuCurrent "main" .) }} active{{end}}">
            <a class="nav-link" href="{{ .URL | relURL }}" title="{{ .Title }}">{{ .Name }}</a>
          </li>
        {{ else }}
          <li class="nav-item dropdown">
            <a class="nav-link dropdown-toggle" href="{{ .URL | relURL }}" id="navbarDropdown" role="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
              {{ .Name }}
            </a>
            <div class="dropdown-menu" aria-labelledby="navbarDropdown">
              {{ range $name, $taxonomy := $currentPage.Site.Taxonomies.tags }}
                {{ with $.Site.GetPage (printf "/tags/%s" $name) }}
                  <a class="dropdown-item" href="{{ .URL | relURL }}">{{ $name }}</a>
                {{ end }}
              {{ end }}
            </div>
          </li>
        {{ end }}
      {{ end }}
    </ul>
  </div>
</nav>
```

We are checking to see if the menu item is tags, and if so look through the tags taxonomy to display the dropdown with all of the items.  We need to go through some hoops to figure out exactly how to get the name and the url, but things are looking good now!

## Adding code highlight

Hugo comes with pygments built in, so we are going to use that with a custom theme.  Lets first generate the css and put it in our themes `static` folder.

```bash
$ hugo gen chromastyles --style=tango > themes/mytheme/static/css
```

And in config.toml add the following lines

```toml
pygmentsCodeFences = true
pygmentsUseClasses = true
```

Finally, add a link to that stylesheet to `themes/mytheme/layouts/partial/head.html`:

```html
  <link rel="stylesheet" href="/css/syntax.css"/>
```


## Content types: post

Since we created a post content type, lets add a different single page template for those to show additional post data.  Create `themes/mytheme/layouts/posts/single.html`:

```go-html-template
{{ define "main" }}
<div class="container">
  <h1 class="mt-5">{{ .Title | markdownify}}</h1>

  {{ if .Params.Subtitle }}
    <h2 class="font-weight-light font-italic mb-3">{{ .Params.Subtitle | markdownify }}</h2>
  {{ end }}

  <p class="text-muted mt-3">
    <a class="text-muted" href="{{ .Permalink }}">Published {{ .Date.Format "January 2, 2006"  }}</a>

    {{ range .Params.tags }}
      <a class="text-muted" href="{{ "/tags/" | relURL }}{{ . | urlize }}">#{{ . }}</a>
    {{ end }}
  </p>

  <article class="article mt-5">
    {{ .Content }}
  </article>
</div>

{{ if or .Next .Prev }}
  <div class="bg-light py-5">
    <div class="container">
      <h2 class="text-center">Read next</h2>

      <div class="row">
        <div class="col-md-6 text-center">
          {{ if .Prev }}
            Previous Post:
            <a href="{{ .Prev.URL | relURL}}">{{ .Prev.Title | markdownify }}</a>
          {{ end }}
        </div>
        <div class="col-md-6 text-center">
          {{ if .Next }}
            Next Post:
            <a href="{{ .Next.URL | relURL }}">{{ .Next.Title | markdownify }}</a>
          {{ end }}
        </div>
      </div>
    </div>
  </div>
{{ end }}

{{ $related := .Site.RegularPages.Related . | first 3 }}
{{ with $related }}
<div class="container mt-5">
  <h2 class="text-center">See also</h2>
  <div class="row">
  	{{ range . }}
      <div class="col">
        <p class="lead mb-0"><a class="text-body" href="{{ .RelPermalink }}">{{ .Title | markdownify}}</a></p>

        {{ if .Params.Subtitle }}
          <p class="lead font-italic mb-0">{{ .Params.Subtitle | markdownify }}</p>
        {{ end }}
        <p class="font-weight-light mt-3">{{ .Summary }}</p>
        <a href="{{ .RelPermalink }}" class="btn btn-primary">Read more</a>
      </div>
    {{ end }}
  </div>
</div>
{{ end }}

{{ end }}
```

Here we are adding some metadata to the top of the page, showing the subtitle if it's set and putting in the published date and some tags.  We are tweaking the layout a bit using the bootstrap utility classes to add some more spacing.  Then the body content which we wrap it inside of an article.

We are showing a few other features that hugo gives us.  One is the `Next` and `Prev` posts in the section, which is by date and content type.  Another is the related pages to this particular page.

### Adding some style tweaks to head.html

Here's some small tweaks to the css to make things a little bit more readable on larger screens. We'll get into really expanding on bootstrap later.

```css
<style>
  .article p, .article ul, .article ol, .article blockquote {
    max-width: 45em;
  }

  .article p:first-child {
    font-size: 1.25em;
    font-weight: 300;
    max-width: 36em; /* 45 / 1.25 */
  }
</style>
```

## Post list view

Adding a special list view for posts pages in `themes/mytheme/layouts/posts/list.html`:

```go-html-template
{{ define "main" }}
{{ $dateFormat := default "Jan 2" (index .Site.Params "date_format") }}
{{ $.Scratch.Set "lastYear" ""}}
<div class="container">
  <h1 class="text-center">{{ .Type | humanize}}</h1>
  {{ range .Pages }}
    {{ $year := .Date.Year }}
    {{ $lastYear := $.Scratch.Get "lastYear"}}
    <div class="row mt-2">
      <div class="d-none d-sm-block col-sm-2 mt-auto offset-sm-2 text-right">
        <time class="post-date" datetime="{{ .Date.Format "2006-01-02T15:04:05Z07:00" | safeHTML }}">{{ .Date.Format "Jan 2, 2006" }}</time>
      </div>
      <div class="col">
        {{ if ne $year $lastYear }}
          <p class="text-muted mt-5">{{ $year }}</p>
          {{ $.Scratch.Set "lastYear" $year }}
        {{ end }}
        <a class="text-body" href="{{ .URL | relURL }}">{{ if .Draft }}DRAFT: {{end}}{{ .Title | markdownify }}</a>
      </div>
    </div>
  {{ end }}
</div>
{{ end }}
```

This uses the `$.Scratch` object to help keep track of the last year that was shown.  When we see a year for the first time, we show a small paragraph with muted text with the year to help split things out.

## Another content type: Photos

Lets add something different -- a photo album.  We'll look at how different content types work, and some image processing options.  First thing we need to do is to grab some photos and create a few albums. I'll let you sort that out.  But create a couple folders under `content/photos` and then an `index.md` file in each of them.  Here's what the directory structure should look like:

```
content/photos
├── bear_mountain
│   ├── 20181006_131733.jpg
│   ├── 20181006_131749.jpg
│   ├── 20181006_131807.jpg
│   ├── 20181006_133024.jpg
│   ├── 20181006_135256.jpg
│   └── index.md
└── fall2018
    ├── 20181020_121433.jpg
    ├── 20181020_153242.jpg
    ├── 20181021_135744.jpg
    └── index.md
```

The `index.md` files should have front matter with at least a title and a data, like this for example:

```
---
title: "Trip to bear mountain"
date: "2018-10-06"
---

This was our trip to bear mountain, super fun.
```

Now if you go to http://localhost:1313 you should see the normal list of pages that is generated with the default list template.  Let's now create a list template for our new photos type in `themes/mytheme/layouts/posts/list.html`:

```go-html-template
{{ define "main" }}

<div class="container">
  <h1 class="text-center">{{ .Type | humanize}}</h1>

  <div class="row">
    {{ range .Pages }}
      {{ $images := .Resources.ByType "image" }}
      {{ $image := index $images 0 }}
      {{ $image := $image.Fill "512x360 smart" }}

      <div class="col-md-6 col-lg-4">
        <div class="card">
          <img class="card-img-top" src="{{ $image.RelPermalink }}">
          <div class="card-body">
            <h5 class="card-title">{{ .Title }}</h5>
            <h6 class="card-subtitle mb-2 text-muted">{{ .Date.Format "Jan 2, 2006" }}</h6>
            <p class="card-text">{{ .Summary }}</p>
            <a href="{{ .RelPermalink }}" class="btn btn-primary">See all</a>
          </div>
        </div>
      </div>
    {{ end }}
  </div>
{{ end }}

```

We are iterating through all of the pages, and then looking for the first image page resource.  Page resources are children of the page, and we are getting the 0 index of it.  We then use [hugo smart image cropping](https://gohugo.io/content-management/image-processing/#smart-cropping-of-images) features to resize that image so it fits into the card.  When you generate the site you will see a new `resources` folder that is part of the root, this is done automatically for you.

Now lets add the page for the gallery itself, in `themes/mytheme/layouts/photos/single.html`:

```go-html-template
{{ define "main" }}

<div class="container">
  <h1>{{ .Title }}</h1>
  <p class="lead">{{ .Content }}</p>

  <div class="row">
    {{ range .Resources.ByType "image" }}
      {{ $image := .Fit "800x600" }}

      <div class="col-md-6 col-lg-4">
        <a href="{{ .RelPermalink }}"><img src="{{ $image.RelPermalink}}" class="img-fluid"></a>
      </div>
    {{ end }}
  </div>
</div>

{{ end }}
```

This resizes the images to fit with in an 800x600 px box and then throws them on a grid.  This is not a great looking gallery but shows a little bit of hugos image processing abilities as well how you can use different layouts for different page types.

Don't forget to edit the `config.toml` if you want to add a link to photos in the header!

## Cleaning up index page

Lets cleanup the homepage.  We are going to add recent posts and start filling in the jumbotron based upon site content.  In the main content part of the site, we are going to create `content/_index.md`  This file will generate the `.Content` that is displayed as well as showing an example of how to pass variables from the front matter into the template itself.

```
---
title: "This is a title"
recentposts: 10
---

Here is a bunch of amazing stuff about what it is that I'd like to see.  It's super amazing.
```

Now for the template in `themes/mytheme/layouts/index.html`.  We first figure out if the `recentposts` parameter is set, and if not we default to 5.  We are also going to only show pages that are of type `posts`.  If you want to show all recent entries on your site, replace `(where .Pages "Type" "posts")` with simply `.Pages`:

```go-html-template
{{ define "main" }}
<div class="jumbotron">
  <div class="container">
    <h1 class="display-4">{{ .Site.Title}}</h1>
    <p class="lead">{{ .Site.Params.Description }}</p>
    {{ .Content }}
  </div>
</div>

<div class="container">
  {{ $recentposts := 5 }}
  {{ if .Params.recentposts }}
    {{ $recentposts = .Params.recentposts }}
  {{ end }}

  <h2 class="text-center">Recent posts</h2>

  {{ range first $recentposts (where .Pages "Type" "posts" ) }}
    <h2 class="mt-5"><a class="text-body" href="{{ .URL | relURL}}">{{ .Title | markdownify}}</a></h2>
    {{ if .Params.Subtitle }}
      <h3 class="font-weight-light font-italic mb-3">{{ .Params.Subtitle | markdownify }}</h2>
    {{ end }}

    <p class="text-muted mt-3">
      <a class="text-muted" href="{{ .Permalink }}">Published {{ .Date.Format "January 2, 2006"  }}</a>

      {{ range .Params.tags }}
        <a class="text-muted" href="{{ "/tags/" | relURL }}{{ . | urlize }}">#{{ . }}</a>
      {{ end }}
    </p>

    <article class="article">
      <p>{{ .Summary }}</p>
    </article>

    <p class="text-muted">Reading time: about {{ .ReadingTime }} minutes</p>
    <p><a class="btn btn-primary" href="{{ .URL | relURL }}">Read more...</a></p>
  {{ end }}

</div>
{{ end }}
```

## Bootstrap advanced theming

Bootstrap is much more interesting when you start using the SASS version of it and are able to tweak the generated code.  Let's look at how we'd get this into our pipeline.  This requires that you have `npm` or `yarn` installed.

Create `themes/mytheme/package.json`:

```json
{
  "name": "mytheme",
  "scripts": {
    "build": "node-sass --include-path node_modules theme.scss static/css/theme.css",
    "watch": "node-sass --watch --include-path node_modules theme.scss static/css/theme.css"
  },
  "dependencies": {
    "bootstrap": "^4.1.3",
    "node-sass": "^4.9.4"
  }
}
```

Then run `npm install`.  Next, lets create a simple `themes/mytheme/theme.scss` file that tweaks some base bootstrap settings:

```scss
$body-bg: #fefefe;
$body-color: #111;

$theme-colors: (
  "primary": #6f42c1,
  "danger": #ff4136
);

$enable-rounded: false;
$enable-shadows: true;
$enable-gradients: true;

@import "bootstrap/scss/bootstrap";
```

Run `npm watch` to look for changes and generate the file as needed.

Then in `themes/mytheme/layouts/partials/head.html` replace the bootstrap that's in the CDN with the version that we just generated:

```html
<link rel="stylesheet" href="/css/theme.css"/>
<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" crossorigin="anonymous"></script>
<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" integrity="sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy" crossorigin="anonymous"></script>
```

And now your site should look slightly different!  I'm not going to go into all of the things that can be tweaked but its really worth looking into what you can do with bootstrap to make it look like you want.

## Overriding a theme file on the local site

When you think about how you want to break things into partials you should consider how the end site might want to override things.  One way is to pass in variables inside of the main site `config.toml` that can switch features on and off, but another way is to override the partials themselves.

The bootstrap header is such an recognizable thing that people might want something all together different, so lets see how we can change that.  Create `layouts/partials/header.html` in the top level site directory:

```go-html-template
<nav class="container my-5">
  <h1 class="pt-5 display-3"><a class="text-dark" href="/">{{ .Site.Title }}</a></h1>
  <p>
    {{ range .Site.Menus.main }}
      <a class="text-dark h4 mr-3" href="{{ .URL | relURL }}" title="{{ .Title }}">{{ .Name }}</a>
    {{ end }}
  </p>
</nav>

```

Now when you refresh the recognizable bootstrap header is gone and a big honking version of your site title is there in place of it!  It's clean.

## Internal templates of Note

Hugo has a bunch of [internal templates](https://gohugo.io/templates/internal/) that can make things easier.  Lets add a few, starting with google analytics.

Inside of `config.toml` set your GA id:

```toml
googleAnalytics = "UA-xxxxx-1"
```

and then in `head.html` adding

```go-html-template
{{ template "_internal/google_analytics_async.html" . }}
```


Also open graph:

```go-html-template
{{ template "_internal/opengraph.html" . }}
```

And twitter cards.  For this you need to add your twitter handle to `config.toml`

```toml
[Social]
  twitter = "wschenk"
```

```go-html-template
{{ template "_internal/twitter_cards.html" . }}
```

_Note_ These interns use a different concept of `.Site.Authors` which I don't understand so it may be easier just to pull out the code as a partial.  

Finally disqus:



## Summary & Recap

1. Themes are composed of `layouts`, `partials`, `single` page templates, `list` templates, and `blocks`.
2. `layouts` `blocks` and `partials` define the structure of templates.
3. `single` and `list` templates are iterating over a collection of elements.
4. There are different page types which can have their own `single` and `list` elements.
5. These can all be overridden by the site using a file if there's no provision for passing in a variable.
6. Pages can have subelements that you can query and pass over.
7. Pages have different representations.
8. You'll need to learn a few hugo variables to get around with things.
9. `range` is how you loop over things.
10. Hugo is pretty fast.
