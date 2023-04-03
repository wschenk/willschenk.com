---
title: Accessing Google Suite from JavaScript
subtitle: who needs servers
tags:
  - howto
  - javascript
  - google suite
date: "2020-02-05"
draft: true
aliases:
  - "/articles/2019/accessing_google_apps_from_javascript"
---

You need to be the Google Suite adminstrator this domain and have access to the DNS server to figure this to make it work.

1. Be a Google Suite Admin
2. Be able to add TXT records to your domain
3. Verify you own subdomain
4. Setup hosting for that domain
5. Enable OAuth Access for Internal Apps
6. Create and Configure the Cloud Apps

## Be a Google Suite Admin

I'm assuming that you already are hosting email and docs on Google App Suite, and that you are an admin.

## Verify you own the domain

The G-Suite domain I'm doing this on is `happyfuncorp.com`, and I'm going to host the code on `directory.happyfuncorp.com`. We are going to grant access to internal apps to OAuth to our Google Suite, and in order to that we need to verify that we own the domain that we are using. If you have Google Suite setup you've probably already verified that you own the top level domain, but we'll need to add the subdomain where we are hosting this expirement.

1. Go to the [Google Suite Admin](https://admin.google.com/)
2. Click on "Domains"
3. Click on "Manage Domains"
4. Enter in your subdomain, in my case `directory.happyfuncorp.com`
5. Follow the directions to add the correct TXT record to in your DNS Hosting provider.
6. Press Verify

## Setup hosting for that domain

There are a zillion ways to do this since we are just going to deal with static files. This is how to do it using [surge](https://surge.sh):

First lets create some basic scaffolds:

```bash
mkdir -p static && cd static
```

{{< highlight "html" >}}
{{% raw "static/index.base.html" "html" "yes" %}}
{{< /highlight >}}

1. Create a A record in your DNS provider, in my case `directory.happyfuncorp.com` pointing to `45.55.110.124`. We'll have to clean this up later.
2. Create a file `static/CNAME` with the domain we want to host on (again `directory.happyfuncorp.com`)
3. Run `npx surge`

Which should prompt you to create an account if you don't have one, and then push the code to the `surge` servers. Verify that it works.

## Create and Configure your Cloud Apps

First we need to make sure that OAuth login works in Google Suite:

1. Go to the [Google Suite Admin](https://admin.google.com/)
1. Open the hamburger menu > Security > App access control
1. At the bottom of the page, check the Trust internal, domain-owned apps box and click Save.

Now we create the app itself:

1. Go to the [Google Cloud Console](https://console.cloud.google.com/)
2. Sign in with your domain account.
3. Create a new project, for example "directory".
4. In the search bar, look for "API & Services"
5. Enable the `People API` by clicking on `+ Enable APIs and Services` on the top, and searching through the list.
6. Enable the `Admin API` also.
7. On the left, select "Domain verification"
8. Add the domain we just setup, e.g. `directory.happyfuncorp.com`
9. Select `OAuth Verification` and select `Internal` and Create
10. Give the app a name, and add the `../contacts.readonly` scope by searching for `People API`.
11. Manually add the `https://www.googleapis.com/auth/admin.directory.user.readonly` scope by selecting manually add, and then paste it in.
12. Add e.g. `directory.happyfuncorp.com` in the authorized domains and save.
13. On the left, select "Credentials"
14. Add an `API Key`. Restrict this to your domain.
15. Add a `OAuth 2 Client ID`, setting both domains to e.g. `https://directory.happyfuncorp.com`

Whew! That was a lot of boring stuff! Lets get on to the code itself.

## Writing code

## References

1. https://support.google.com/a/answer/7281227
1. https://cloud.google.com/storage/docs/hosting-static-website
1. https://support.google.com/a/answer/6343701
1. https://developers.google.com/people/quickstart/js
1. https://developers.google.com/people
