---
title: 'Static apps are cool'
subtitle: 'building scalable sites'
tags: middleman, ruby, howto, static_sites
header_image: tesla_coil.jpg
---

I feel bad for servers constantly doing a whole lot of busy work with the same result, burning up energy doing the same dumb thing over and over.  Relatively simple sites hosted on wordpress explode on the slightest traffic spike, and what they are serving up is basically static content.   The admin tools are nice, but you end up going deep down the hole of caching, layers of Varnish slapped over PHP Caching, with CNDs out in Cloudfront to distribute the traffic, database servers desperatly needing indexes, the full catastrophe.

We can look at each of the elements on a website and break down the type of data source it came from:

- Static Data: Templates, Images, Fonts, Assets
- Site Data: Product Lists, Categories, Pages, Articles
- Dynamic Data: Inventory, Comments
- Session Data: User name, Items in cart, Likes, etc.

As we go down the list, the rate in which these things change increase.  Static data, as the name implies, rarely changes.  Templates, with their associated content, are created during the site development process, and once the site is deployed they change only in response to bug fixes or feature requests.

Site Data changes during the operations of the business, but not very frequently and generally in response to things that outside of the site per se.  These include things like marketing material, product pages (less inventory information), categories, articles, and really the bulk of the content on a site.  There are many reasons why you want to generate these pages using some sort of CMS, but rarely does this data change at _run time_.  This stuff doesn't really need to be in a database as such, but it's convient to stick this stuff there if you have one laying around.

Dynamic Data is _run time_ data, that is to say, things that change as part of site operations, things like Orders and credit card transactions.  Most _User Generate Content_ falls in the this category, and depending upon your feature set, that could be the majority of the pages.  Unlike Site Data, where it's a good idea to use a database, this is _neccessarily_ stored in some sort of database.

Session Data is _specific to the user_, and renders the page differently based upon there session, such as their name, whether they've added an item into the cart or not, etc.  This gets persisted from page to page, but generally can get thrown away if the user wanders off.

## Most sites are static and site data



_Image Credit: [Peretz Partensky](https://www.flickr.com/photos/ifl/14916917569/in/photolist-oJa6TX-P5AzF-4Vga7e-exjV6s-bPYEUa-bCqBNr-8d2Hi9-8cYpjR-8d2FT3-8cYpHM-8cYp7e-8cYqqx-aDJBoj-9xJtWM-4yt7kS-4WpvGW-9g5VCK-bCqfRV-9yEigG-9uUmVQ-9uRmLK-9uRjNp-9xAkbU-bpvmhS-bpvmDm-bpvk6b-a31bV-bpvmQW-bCqCzT-bpvFwh-bCqgCz-bpvH3d-bpvF8E-bCqJJT-PLUBa-9yMsKF-9yMrST-9yQtxS-9yQt6d-PLjKy-PLk9o-PLU4X-PLUHx-o5E7w-8FRP5h-8FRNYY-8FNCrt-9uRqhv-PLUrt-9uRonF)_

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
##### 
File Data Store -> File Template -> Layout -> Final HTML

CSS:
SASS/CSS -> CSS -> Minifier

Javascripts:
CoffeScript -> JavaScript -> Compression

Browser:
Load HTML -> Process HTML -> Load Assets -> Layout Page -> Execute Client Code

