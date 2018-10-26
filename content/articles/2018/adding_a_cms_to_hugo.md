---
title: "Adding a CMS to hugo"
date: 2018-10-26T11:26:54Z
tags:
  - hugo
  - howto
  - static_sites
  - netlify
draft: true
---

https://www.netlifycms.org/docs/add-to-your-site/

in `/static/admin/index.html`:

```html
<!doctype html>
<html>
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>Content Manager</title>
</head>
<body>
  <!-- Include the script that builds the page and powers Netlify CMS -->
  <script src="https://unpkg.com/netlify-cms@^2.0.0/dist/netlify-cms.js"></script>
</body>
</html>
```

Then in ``/static/admin/config.yml`:

```yml
backend:
  name: github
  repo: wschenk/willschenk.com

publish_mode: editorial_workflow

media_folder: static/img/uploads
public_folder: img/uploads

collections:
  - name: "articles"
    label: "Articles"
    folder: "content/articles"
    create: true
    fields:
      - { label: "Title", name: "title", widget: "string" }
      - { label: "Date", name: "date", widget: "date" }
      - { label: "Body", name: "body", widget: "markdown" }
```

## What if I don't have a netlify account?

There are a number of ways to do this, but we are going to

1. [Create a GitHub application](https://developer.github.com/apps/building-oauth-apps/creating-an-oauth-app/) that will do the granting
2. Create a firebase application to handle the accounts
3. Wire everything together.

### Creating the GitHub app

Go to [GitHub developer settings](https://github.com/settings/developers).  You can access this by clicking on your profile, selecting `Settings`, and then selecting `Developer Settings`.

Create a `New OAuth App`.

Leave the callback URL blank for now -- we'll set that up with a url that we will get from firebase shortly.

*Take note of the client ID and client secret, we'll need those shortly*

### Creating the firebase application

We will spin up something on firebase to help handle the oauth dance.

First lets checkout some code that will handle the api interactions.  We are going to install this code on firebase.

```bash
$ git clone https://github.com/Herohtar/netlify-cms-oauth-firebase
$ cd netlify-cms-oauth-firebase/functions/
$ npm i
$ cd ..
```

Then install  `firebase-cli` if you haven't already

```bash
$ npm add -g firebase-tools
```

Next log in to firebase

```bash
$ firebase login
```

Go to the [firebase console](https://console.firebase.google.com/) and create an application.  **Upgrade the billing plan to something that is paid, for example Blaze**.  You will be will under the usage limits before you get charged but without this the final auth callback will not work.

Then configure firebase to use that project

```
$ firebase use projectname
```

Now lets setup the client id and secret that we got from GitHub:

```
$ firebase functions:config:set oauth.client_id=yourclientid oauth.client_secret=yourclientsecret
```

This will take a few seconds to finish.  Now we can deploy our functions, and get the URL that GitHub will use to pass back the authentication.

```bash
$ firebase deploy --only functions
```

Note the url of the application deployed, in my case it is `https://us-central1-willschenkcom.cloudfunctions.net/oauth`

### Writing it all up

Go back to the GitHub app page and change your app's callback to the hostname/oauth/callback -- in my case `https://us-central1-willschenkcom.cloudfunctions.net/oauth/callback`.

Then in `static/admin/config.yml` set the `baseURL` to your firebase application.  In my case the relavent section looks like:

```yml
backend:
  name: github
  repo: wschenk/willschenk.com
  base_url: https://us-central1-willschenkcom.cloudfunctions.net
  auth_endpoint: /oauth/auth
```

That should be everything!

## Automating the builds of the site

I've never used the netlify platform but it seems really interesting.
