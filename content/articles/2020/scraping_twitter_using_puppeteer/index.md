---
title: Scraping your twitter data
subtitle: keep your posts local
tags:
  - howto
  - scraping
  - puppeteer
date: "2020-01-06"
draft: true
---

Twitter famously leveraged their open API to build an ecosystem that helped it grown, and then killed then crippled that API once it needed to make money leaving a lot of developers out there feeling like suckers.  Such is the fate you tempt when using someone else's platform. So lets look at how to use twitter programmatically without asking their permission. Maybe this time they won't take it away.

## Using Puppeteer

[Puppeteer](https://pptr.dev/) is a node.js interface to headless Chrome -- this means that we can write a script, that runs on our computer, that makes Chrome do things. The reason that this exists is for testing, so when you are writing a piece of software you can run it in a very similar environment to how the use will experience it, with network delays, imaging rendering, javascript runtimes and all. Conceptually, we are going to "write tests" against "twitter", and instead of checked to see if the "results are correct" we are going to write them to a file.

First setup your project.

```bash
mkdir twitter
cd twitter

npm init -y
yarn add puppeteer delay mkdirp
```

## Loading a profile

Lets go through some code that starts up chrome, then navigates to a profile page and pulls out the relevant information from a profile.

1. `await puppeter.launch({ headless: true});` starts up a browser, change `true` to `false` to watch it all happen.
2. `page.goto` vists the page.
3. `page.waitForSelector` waits until the DOM has been instantiated with that selector
4. `page.evaluate` runs code within the browser's context, so we can use CSS selectors to pull out data values.

From here, we'll save it in `output/${screename}/profile.json`

[`profile.js`](`profile.js`)
{{% code file="articles/2020/scraping_twitter_using_puppeteer/profile.js" language="js" %}}

You'll need to pass in the username that you are looking for in an environment variable, for example:

```bash
$ TWITTER_USER=wschenk node profile.js
```

## Parsing tweets

Next lets look at pulling out the tweets from the rendered HTML. Twitter's HTML is... interesting.  Lets first download the HTML so we can look at it in our browser. This is how the Chrome browser interprets the data, which is different than what I see using the developer tools.  It seems that you get a different version of the twitter front end based on whether you are logged in or not.

[`timeline_dump.js`](`timeline_dump.js`)
{{% code file="articles/2020/scraping_twitter_using_puppeteer/timeline_dump.js" language="js" %}}

Running this will create a file called `timeline.html` inside of the output folder, which we can then open up.

```bash
$ TWITTER_USER=wschenk node timeline_dump.js
```

This gives us a good view of what the rendered HTML looks like after JavaScript does it's thing.

## Printing out the tweets

[`timeline.js`](`timeline.js`)
{{% code file="articles/2020/scraping_twitter_using_puppeteer/timeline.js" language="js" %}}

## Scrolling to past pages

## Logging in

## Session saving
