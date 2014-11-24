---
title: "Pulling data out of Google Analytics"
subtitle: "see who's talking about your stuff"
tags: socialinvestigator
published: false
---

I like staring at the real time stats of [Google Analytics](http://www.google.com/analytics/).  As a dashboard, it's not really as amazing as [Chartbeat](https://chartbeat.com) is, and it doesn't let you drill down into the data as much as [Mixpanel](https://mixpanel.com).  But GA is super simple to setup and its Google, so everyone uses it.

Another obsessive/fun thing to do is to see where that spike in inbound traffic is coming from.  On [HappyFunCorp](http://happyfuncorp.com) there are days where we get a sudden influx of [Happy Thoughts](http://happyfuncorp.com/#happy-thoughts) which warms our hearts and floods our inboxes.  Where did they come from?  How do we figure it out?

Lets look at how we can interact with Google Analytics using [google-api-ruby-client](https://github.com/google/google-api-ruby-client).  At the end of this, we are going to be able to see the current traffic stats, top referrals, see a timeline of when the referals first came in, and do what we can from that information to track down who is talking about us.  GA will show us that we are getting a lot of `SOCIAL` traffic, but what else can we figure out?

# Step 1: OAuth: Setting it up to access Google on behalf of the user

We're going to be using OAuth2 to authorize our script.  So head over to the [Google Developers Console](https://console.developers.google.com/project).

1. *Create a Project*.  You should name this something that makes sense to you.  I called my _Social Investigator_

2. Enable the _Analytics API_.  This can be done on the side bar, under **APIs and auth > Apis**.  Scroll down to find it.

3. **APIs and auth > Consent Screen**.  Create something here, you'll need to flesh this out later.

4. **APIs and auth > Create Client ID**.  Select **Installed Application** with type _Other_.  This will create the keys for you, and then you **Download JSON** and save it in a file.

