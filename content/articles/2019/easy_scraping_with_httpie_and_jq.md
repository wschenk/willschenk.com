---
title: Easy scraping with httpie and jq
subtitle: Pulling my GitHub starred repositories into Hugo
tags:
  - howto
  - hugo
  - scraper
  - jq
date: 2019-04-18
---

I recently saw a tweet mentioning the combination of using [HTTPie](https://httpie.org/) (a command line HTTP client), [jq](https://stedolan.github.io/jq/) (a lightweight and flexible command-line JSON processor) and [Gron](https://github.com/tomnomnom/gron) (Make JSON greppable!) was "all you needed to build a scraper."  Lets see if that's true.

## First install

Linux:

```bash
sudo apt-get install httpie jq
```

OSX

```bash
brew install httpie
brew install jq
```

## Lets get some stars

We can pull down the list of repositories people have starred on Github using the URL scheme `https://api.github.com/users/:username/starred`.  Try this:

```bash
http https://api.github.com/users/wschenk/starred
```

This returns a wall of data!  `HTTPie` does a nice job of formatting the JSON out and showing the response headers.

Let's use `jq` to print out the first response:

```bash
http https://api.github.com/users/wschenk/starred | jq '.[0]'
```

 Weirdly, the Github API returns the items in the order starred, but doesn't include the date that we starred them.  There's an [alternative api call](https://developer.github.com/v3/activity/starring/#alternative-response-with-star-creation-timestamps-1) that we can use to get the date that I pushed the Star button.  For this to work we need to pass in an Accept header of `application/vnd.github.v3.star+json` which is done like so:

 ```bash
 http https://api.github.com/users/wschenk/starred Accept:application/vnd.github.v3.star+json| jq '.[0]'
 ```

`jq` lets us slice and dice the JSON.  Lets pull out the fields that we want.  We are using the `.[]` syntax and piping it to a json writer, which is pulling only the fields out that we want.  And we write the output to the data directory in our hugo app.

```bash

http https://api.github.com/users/wschenk/starred \
  Accept:application/vnd.github.v3.star+json | \
  jq '[.[] | {name: .repo.name, full_name: .repo.full_name,
  avatar_url: .repo.owner.avatar_url, login: .repo.owner.login,
  description: .repo.description, starred_at: .starred_at,
  html_url: .repo.html_url, language: .repo.language,
  updated_at: .repo.updated_at}] ' > data/stars.json

```

## Using the hugo data directory

Now that we have the data in a JSON format that Hugo can understand, lets build a page to render this glory.

Create a simple file in `content/stars.md` that we'll use to define a kind of `stars`

```md
---
title: GitHub stars
type: stars
---

I like software.  Here is some of the stuff I've found on GitHub that
I've marked as interesting.
```

Now we create a single page template that we'll use to render it in `layouts/stars/single.html`:

```go-html-template
{{ define "main" }}
<div class="container">
  <h1 class="display-4 text-center">{{.Title}}</h1>

  {{ .Content }}

  <ul class="list-unstyled">
  {{ range $.Site.Data.stars }}
    <li class="media my-2">
      <img class="mr-3 img-fluid" width="64px" src="{{ .avatar_url }}" alt="{{ .owner }} avatar">
      <div class="media-body">
        <h4 class="mt-0 mb-1">
          <a href="{{.html_url}}">{{ .name }}</a>
          <small><span class="ml-1 text-muted">{{ .language }}</span></small>
        </h4>
        <p class="lead my-0">{{ .description }}</p>
        <p class="text-muted">
          Starred {{ dateFormat "Jan 2, 2006" .starred_at }},
          Updated {{ dateFormat "Jan 2, 2006" .updated_at }}
        </p>
      </div>
    </li>
  {{ end }}
  </ul>
</div>

{{ end }}
```

Now start up your server and go to http://localhost:1313/stars !

## Less easy scraping

So it turns out that I've been liking lots of things on GitHub and I have more than one page of results.  The GitHub api used the HTTP `Link` header to point to `next`, `prev`, `first`, and `last` last pages.  We'll write a little script that saves that and then parses it out.

1. We create a small function that passes in `-dh` flags to `http`, redirecting stdout to our file `>page1.json` and stderr to a file containing the headers `2>headers`.
2. Then we'll parse the headers using grep, tr, and sed to pull out the url that's matched with `rel="next"`.
3. If there is one we'll follow that and download `page2.json` etc.
4. Then we'll merge all of the files together using `jq --slurp '[.[][]]' *json` so the muliple files of JSON Arrays is one big JSON array.
5. And then copy over our existing `jq` parsing

Here's `update_github_stars.sh`:

```bash
#!/bin/bash

# Download a url
function scrape_page() {
  echo Downloading $1 to $2
  http $1 Accept:application/vnd.github.v3.star+json -dh >$2 2>$3
}

function next_link_from_header() {
  # grep - Filter for the Link header
  # tr - slit each of the entries into it's own line
  # grep - look for the next link
  # sed - get rid of everything out side of the < >
  cat $1 | grep ^Link |tr ',' '\n'|grep \"next\" | sed 's/.*<\(.*\)>.*/\1/'
}

OUTDIR=$(mktemp -d)
PAGE=1
NEXTURL=https://api.github.com/users/wschenk/starred

while [ ! -z "${NEXTURL}" ]; do
  scrape_page $NEXTURL  $OUTDIR/page${PAGE}.json $OUTDIR/headers

  NEXTURL=$(next_link_from_header $OUTDIR/headers)
  PAGE=$(( $PAGE + 1 ))
done

echo Finished scraping pages

# Combine the files
# Parse them into data/stars.json

jq -s '[.[][]]' $OUTDIR/*json | \
  jq '[.[] | {name: .repo.name, full_name: .repo.full_name,
  avatar_url: .repo.owner.avatar_url, login: .repo.owner.login,
  description: .repo.description, starred_at: .starred_at,
  html_url: .repo.html_url, language: .repo.language,
  updated_at: .repo.updated_at}] ' > data/stars.json
```

## Conclusion

My [Star List]({{< relref "/stars.md" >}}) is a little unwieldy right now with 120 entries (`js '. | length' data/stars.json`) and not all of the descriptions are that informative, but this was all built with simple tools and minimal dependacies.  We didn't need any special libraries to script this, `Gemfile` or `package.json` installations, just a relatively simple bash script.
