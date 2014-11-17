---
title: 'Building a Middleman Blog'
subtitle: "Static publishing"
tags: middleman, ruby
---

We can look at each of the elements on a website and break down the type of data source it came from:

- Static Data: Templates, Images, Fonts, Assets
- Site Data: Product Lists, Categories, Pages, Articles
- Page Data: Comments, Popular Posts
- User Data: User name, Items in cart, Likes, etc.

As we go down the list, the rate in which these things change increase.  Static data, as the name implies, rarely changes.  Templates, with their associated content, are created during the site development process, and once the site is deployed they change only in response to bug fixes or feature requests.

Site Data changes slightly more often and in response to things that outside of the site per se.  These include things like who
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

