---
title: "Automating hugo builds using CircleCI"
subtitle: "Let someone else run your build server"
date: 2018-10-29
tags:
  - static_sites
  - hugo
  - howto
---

Here's a simple CircleCI configuration to pull down the latest version of your hugo site on GitHub commits, build it, and then push it to github pages.

Let's set that up, following [their documentation on how configure CircleCI to build static sites](https://circleci.com/blog/automate-your-static-site-deployment-with-circleci/).

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
