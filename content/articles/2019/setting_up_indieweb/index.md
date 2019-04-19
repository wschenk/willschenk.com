---
title: "Setting up Indieweb Homepage"
subtitle: "the dream of the nineties is alive on the indieweb"
tags:
  - howto
  - indieweb
  - p2p
  - microformats
date: "2019-04-19"
---

Remember [microformats](http://microformats.org/)?  Me neither!
Back when the web was open and we were trying to find ways to interconnect independent things?
Let's bring them back!
<!--more-->

## Steps: Simple

1. Get a domain

2. Host your own site on it

3. On the index page, create an `h-card`

4. Inside of that `h-card` an identity markers.

5. That's it.

## Example

You should replace this stuff with your own. Check out [the standard's documentation](http://microformats.org/wiki/h-card)
for more details.

```html
<div class="h-card">

  <h1 class="p-name">Will Schenk</h1>
  <img class="u-photo" src="photo" width="300">

  <p>
    I work at
    <a href="https://happyfuncorp.com" class="p-org">HappyFunCorp</a>
    where I am a
    <span class="p-job-title">software craftman</span>
  </p>

  <p>
    <ul>
      <li><a href="https://willschenk.com" class="u-url u-uid" rel="me">Will Schenk</a></li>
      <li><a href="mailto:wschenk@gmail.com" class="u-email" rel="me">wschenk@gmail.com</a></li>
      <li><a href="https://twitter.com/@wschenk" rel="me">Twitter</a></li>
      <li><a href="https://instagram.com/wschenk" rel="me">Insta</a></li>
      <li><a href="https://linkedin.com/in/will-schenk-420266" rel="me">LinkedIn></a></li>
    </ul>
  </p>
</div>
```

You should obviously replace that information with your own.

## Testing

After you deploy, the easiest way to test is to go to https://indiewebify.me/validate-h-card/

So do that.

But lets build a simple tester so we can see how to interact with it.


```bash
$ mkdir indieweb
$ cd indieweb
$ npm init
$ yarn add microformat-node node-fetch
```

Then create `index.js` to pull it down and print it out:

{{% code file="content/articles/2019/setting_up_indieweb/index.js" language="js" %}}

And you should see the nicely parsed information!  Now what to do with this all!
