//= require 'jquery'
//= require 'bootstrap-sprockets'

$( function() {
  $("#comment_shower").click( function(e) {
    e.preventDefault();
    $(".comment_universe").slideToggle();
    return false;
  } );

  $("#comment_shower_scroll").click( function(e) {
    $(".comment_universe").slideToggle();
  } );

  $("img[alt=\"left_float\"]").addClass( "left_float" );
  

  $( "#search_box" ).on( 'input', search_typing );
} );

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