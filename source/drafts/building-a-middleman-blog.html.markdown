---
title: 'Admin tools for static sites with Middleman and Angular'
subtitle: 'adding a CMS to static apps'
tags: middleman, ruby, angular
---

I feel bad for servers constantly doing a whole lot of busy work with the same result, burning up energy doing the same dumb thing over and over.  Relatively simple sites hosted on wordpress explode on the slightest traffic spike, and what they are serving up is basically static content.   The admin tools are nice, but you end up going deep down the hole of caching, layers of Varnish slapped over PHP Caching, with CNDs out in Cloudfront to distribute the traffic, database servers desperatly needing indexes, the full catastrophe.

But the admin tools are nice.


We can look at each of the elements on a website and break down the type of data source it came from:

- Static Data: Templates, Images, Fonts, Assets
- Site Data: Product Lists, Categories, Pages, Articles
- Dynamic Data: Inventory, Comments
- User Data: User name, Items in cart, Likes, etc.

As we go down the list, the rate in which these things change increase.  Static data, as the name implies, rarely changes.  Templates, with their associated content, are created during the site development process, and once the site is deployed they change only in response to bug fixes or feature requests.

Site Data changes during the operations of the business, but not very frequently and generally in response to things that outside of the site per se.  These include things like who
- 
Server Template: ERB, HAML
Server Template Components: Layouts, Partials
Client Templates: HTML
Client Templates Stategies: Direct DOM, Mustache, JSON, Local Storage

Data Store: File system, Site Data (Products, Posts), Page Data, Session Data (User, Views), Database
Client Data Store: DOM, Backbone, Angular, etc.


Complete HTML:
User/Session Info -> Data Store -> Filter -> Partials -> File Template -> Layout -> Final HTML

CSS:
SASS/CSS -> CSS -> Minifier

Javascripts:
CoffeScript -> JavaScript -> Compression

Browser Environment:
Load HTML -> Process HTML -> Load Assets -> Layout Page -> Execute Client Code


---

Static Sites

File Data Store -> File Template -> Layout -> Final HTML

CSS:
SASS/CSS -> CSS -> Minifier

Javascripts:
CoffeScript -> JavaScript -> Compression

Browser:
Load HTML -> Process HTML -> Load Assets -> Layout Page -> Execute Client Code

