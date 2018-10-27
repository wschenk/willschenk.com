---
title: "Adding a CMS and build server to hugo"
date: 2018-10-27
tags:
  - hugo
  - howto
  - static_sites
  - netlify
---

Just because we have a static site doesn't mean that we can't have an admin tool to write and edit posts!  Lets go through how we can [add the NetlifyCMS](https://www.netlifycms.org/docs/add-to-your-site/) to the site and host it wherever we want.

In my case I'm storing the code on [GitHub](https://github.com) and also serving the pages from [GitHub Pages](https://pages.github.com/).  [Netlify](https://www.netlify.com/) also seems like a really promising company with a number of other services that they offer, so I'd encourage you to check it out.  But since I'm changing one thing at a time, I'll leave that for a later exercise.

The CMS works by loading the code and it's configuration from your site.  These are 100% static so nothing needs to happen at build time.  The configuration tells the admin -- which again runs totally in your browser -- where to get authentication and the data for your site.  Once that's done you'll need to figure out how to setup the build process.

## Install the admin

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

Then install  `firebase-tools` if you haven't already

```bash
$ npm add -g firebase-tools
```

Next log in to firebase

```bash
$ firebase login
```

Go to the [firebase console](https://console.firebase.google.com/) and create an application.  Then configure firebase to use that project

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

### Wiring it all up

Go back to the GitHub app page and change your app's callback to the hostname/oauth/callback -- in my case `https://us-central1-willschenkcom.cloudfunctions.net/oauth/callback`.

Then in `static/admin/config.yml` set the `baseURL` to your firebase application.  In my case the relavent section looks like:

```yml
backend:
  name: github
  repo: wschenk/willschenk.com
  base_url: https://us-central1-willschenkcom.cloudfunctions.net
  auth_endpoint: /oauth/auth
```

## Automate builds using CircleCI

Now that we are able to edit the content of the site, we'll need a way to build it.  I'm going to use CircleCI to trigger the build.  Let's set that up, following [their documentation on how configure CircleCI to build static sites](https://circleci.com/blog/automate-your-static-site-deployment-with-circleci/).

Create `.circleci/config.yml`

```yml
version: 2
jobs:
  build:
    branches:
      ignore:
        - gh-pages
    docker:
      - image: cibuilds/hugo:latest
    working_directory: ~/hugo
    environment:
      HUGO_BUILD_DIR: ~/hugo/public
    steps:

      # install git
      - run: apk update && apk add git

      # checkout the repository
      - checkout

      # install git submodules for managing third-party dependencies
      - run: git submodule sync && git submodule update --init

      # Link the public dir to the gh-pages branch

      - run: rm -fr $HUGO_BUILD_DIR && git worktree add -B gh-pages $HUGO_BUILD_DIR origin/gh-pages

      # build with Hugo
      - run: HUGO_ENV=production hugo -v -d $HUGO_BUILD_DIR

      # Set some variables to add to the commit message
      - run: git config --global user.email "noreply@example.com" && git config --global user.name "CircleCI Bot"

      # Push the generated files back to github
      - run: cd $HUGO_BUILD_DIR && git add --all && git commit -m "Automated publish to gh-pages" && git push
```

Commit and add this file to your reposity on GitHub.  This runs the hugo build image.  It installs git, and then checkout the repo from GitHub.  We pull down any submodules if you are using that for themes.  We then configure the `$HUGO_BUILD_DIR` to be a `worktree` of the `gh-pages` branch -- this is where we are going to host the files in GitHub pages.  Then it runs the build itself, adds and commits those files in the `gh-pages` branch back into the repository, and pushes back to `GitHub`.

For this to work you need to grant `CircleCI` write access to your repo which is done by setting it up with your user's key.  Lets get CircleCI working now.

1. Go to [CircleCI](https://circleci.com/) and create an account.
2. Select the single linux container plan.
3. Add your repository from GitHub that you want to build.  
4. Go to project settings, and under Permissions go to `Checkout SSH Keys`.  
5. `Add User Key` to grant permission.
6. Remove the previous deploy key.

Now when you push your commits to `GitHub`, you'll be able to watch CircleCI build them and hopefully see your new content go up shortly!

## Summary

The NetlifyCMS is really interesting, though its still a work in progress.  With this setup, we are using a couple of external services -- GitHub, Firebase, CircleCI -- all within their free tiers to edit and push out a website. This process would probably be even easier if we went all in with Netlify, so that's another great service to start checking out.
