---
title: Adding search to a middleman blog
subtitle: slightly simplier than google
date: 2015-06-07 14:09 UTC
tags: middleman, howto, ruby
---

We're going to build a simple, niave search for middleman blogs.  We're going to build a _search index_ at build time, and then use that index to perform the search itself on the client side.

## Building the index

When you typed in something in google, it doesn't then go and hit every page on the internet to check to see if there's a match.  It doesn't even look at every page that it has squirreled away somewhere in the googleplex.  What it consults is an _index_ of the documents out there, and the index points to the page information.  (We all know that it's a lot more complicated than that really, but run with it.)

First thing we're going to do is create a very simple version of this index for your site.  This is going to be in a file called `source/article.index.json.erb`.

1. Go through all of the articles.
2. Add meta data for the article into the master map.
3. Find all of the words in the article, by stripping out all of the html tags, making things lowercase, and breaking it apart by white space.
4. Insert all of those words into our index.
5. Convert the whole sucker to JSON.


```erb
<%
  map = {articles:{}}
  index = {}
  blog.articles.each do |article|
    map[:articles][article.url] = {
      title: article.title, 
      date: article.date, 
      tags: article.tags 
    }

    words = "#{article.title} #{article.body}"
    words = words
      .downcase              # make lowercase
      .gsub( /<.*?>/, "" )   # get rid of tags
      .gsub( /[^\w ]/, "" )  # get rid of not letters
      .split( /\s+/ )        # split by words
      .sort.uniq

    words.each do |w|
      index[w] ||= []
      index[w] << article.url
    end
  end
  map[:index] = index
%>
<%= map.to_json %>
```

## Now lets add some markup to the blog

I'm sticking this in the header, as you see above:

```haml
%form.navbar-form.navbar-right#search{ role: "search" }
  .form-group.dropdown
    .input-group
      %input.form-control#search_box{ type: "text", placeholder: "Search", autocomplete: "off" }
      %span.input-group-btn
        %button.btn.btn-default
          %span.glyphicon.glyphicon-search
    %ul.dropdown-menu.dropdown-menu-left.results
      = link_to "Title", "/url"
```

## Loading and Querying the index

Ok, lets build this from the ground up.  All this goes into `application.js`.First we're create a method that loads up the index if we need it.  We're going to use a promise here, so if multiple request come in at the same time only one will go to the server:

```js
var article_index = function() {
  if( window.article_promise === undefined ) {
    console.log( "Loading article index" );
    window.article_promise = $.Deferred();

    $.get( "/articles.index.json" ).done( function(data) {
      console.log( "Loaded article index" );
      window.article_promise.resolve( data );
    } );
  }

  return window.article_promise;
}
```

This is called like `article_index().done( function( index ) { console.log( index )})`  The second time it calls, it returns the first promise again so everything is nice and fast.

To query the index itself we need to look through all of the words and return a list of urls that match:

```js
var match_index = function( word, index ) {
  var result = [];
  var matcher = new RegExp( word );

  $.each( index.index, function( i ) {
    if( matcher.exec( i ) ) {
      $.each( index.index[i], function( idx, url ) {
        if( result.indexOf( url ) < 0 ) {
          result.push( url );
        }
      } );
    }
  });

  return result;
}
```

Now lets build a simple search.  This is a little complicated, since we need to compute the intersection of the results if the user types in multiple words.  Here's what's happening:

1. We create a promise, since we may need wait for the index to load.
2. We split the search term into multiple words.
3. Collect the results of the `match_index` function.
4. Compute the intersections of all the results
5. Look up the meta data based on the url.
6. Resolve the promise with the results.

```js
var find_article = function( search ) {
  var search_results = $.Deferred();

  article_index().done( function( index ) {
    // Split the search by widespace
    var words = search.toLowerCase().replace(/\s+$/, '').split( /\s+/ );

    // Lookup the matches for each word
    // Note using $.map seems to flatten the result.
    var full_results = [];
    $.each( words, function( i, word ) {
      full_results.push( match_index( word, index ) );
    } );

    var urls = full_results[0];

    // If there are multiple words, compute the intersection
    if( full_results.length > 1 ) {
      var url_counts = {}
      $.each( full_results, function( i, set ) {
        $.each( set, function( i, url ) {
          url_counts[url] = (url_counts[url]||0)+1;
        } )
      } );

      urls = [];
      $.each( url_counts, function( url, count ) {
        if( count == full_results.length ) {
          urls.push( url );
        }
      } );
    }

    // Pull in the metadata
    var results = {};
    $.each( urls, function( i, url ) {
      results[url] = index.articles[url];
    } );

    search_results.resolve( results );
  });

  return search_results;
};
```

## Wiring it up

First we need to call our code when the user inputs something in the text area:

```js
$(function() {
  $( "#search_box" ).on( 'input', search_typing );
});
```

Then we wire everything together:

1. If the field is empty, hide the dropdown.
2. Otherwise show the dropdown and a loading message
3. Call `find_article` and when it returns
4. Put the results in the result dropdown.

```js
var search_typing = function() {
  var val = $(this).val();

  if( val == "" ) {
    $("#search .dropdown").removeClass( "open" );
  } else {
    $("#search .dropdown").addClass( "open" );
    $("#search .dropdown-menu").html("<li class='disabled'><a href='#'>Loading " + val +"</a></li>")
    find_article( val ).done( function(results) {
      var menu = $("#search .dropdown-menu").html("");
      if( $.isEmptyObject( results ) ) {
        $("#search .dropdown-menu").html("<li class='disabled'><a href='#'>No results for: " + val +"</a></li>")
      }
      $.each( results, function( url, info ) {
        menu.append( "<li><a href=\"" + url + "\">" + info.title + "</a></li>");
      });
    });
  }
}
```

## Next steps

1. Stemming
2. Logical operations
3. Showing more metadata in the search results.
