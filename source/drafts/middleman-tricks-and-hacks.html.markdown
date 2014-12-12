---
title: 'Middleman Tricks and Hacks'
subtitle: 'Things I learned rebuilding this site'
tags: middleman, ruby, howto, tools
---

As part of the process of getting this site to work, I learned some more things about how to better build a site with middleman.  Building out off our [foundational article](/building-sites-with-middleman/) here are a few other things that I found very useful.

## Partials

The `index.html.haml`, `articles.html.haml`, `tag.html.haml` and `calendar.html.haml` pages all use the same partial to list out the post archives, which are mostly the same.

On the _index_ page it's called like this, where I'm supressing the date heading:

```haml
= partial "post_list", :locals => {:page_articles => blog.articles[1..4], :no_date => true }
```

and in the _articles_ I'm including draft posts for my own reference.

```haml
= partial "post_list", :locals => {:page_articles => (drafts + page_articles)}
```

The `_post_list.haml` file then has some runtime logic to show what it needs to.

```haml
- last_date = nil
- no_date = !!no_date
%ul
  - page_articles.each do |current_post|
    - if !no_date && current_post.date
      - date_string = current_post.date.strftime('%b %Y')
      - if last_date != date_string
        %li.date
          %h2= date_string
      - last_date = date_string
    %li
      .more
        - unless current_post.is_a? ::Middleman::Blog::Drafts::DraftArticle
          = current_post.date.strftime( '%b %e' )
        - else
          Draft
      %div= link_to current_post.title, current_post
      %div
        = current_post.data['subtitle']

        .tags
          - current_post.tags.sort.each do |tag|
            .tag= link_to tag, tag_path( tag )
```

Since I use [semantic CSS classes](/bootstrap-advanced-grid-tricks/) to define my layouts, this works really well if we want to layout things differently on different pages.

## Layouts and partials for article layout

Middleman posts are generally written in markdown, which translates into a series of `<p>` tags that can thrown into a layout file.  In order to create the table of contents on the left, the navigation to other articles on the right, and the unique header and footer, I used a seperate `article_layout`.  Setting up **Scrollspy** and **Affix** means we need to change things on the `<body>` tag that we don't need to do for other pages, so it makes more sense to use a seperate file here rather than a _nested layout_.

This means that all the things that are shared between the two layouts, the main layout for all the meta pages and the article layout for the content pages, should be factored into partials.  I put these partials in the `layouts/` directory.

## Markdown toc_data
## Helpers that parse the source file

## Directory index
## Site data as database
## url_for in bootstrap-navbar
## Helpers to do databasey things
