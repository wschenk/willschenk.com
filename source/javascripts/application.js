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

  // if( $(".sidebar").size() > 0 ) {
  //   $("[data-offset-top]").attr( "data-offset-top", $(".sidebar").offset().top );
  // }

  $("img[alt=\"left_float\"]").addClass( "left_float" );
  
  // $.fn.originalTrigger = $.fn.trigger;
  // $.fn.trigger = function(type, data) {
  //   console.log(typeof(type)=='string' ? type : type.type, data);
  //   return $(this).originalTrigger.apply(this,arguments);
  // }

  $("#search_box").on( 'input', function(data) {
    var val = $(this).val();

    if( val == "" ) {
      $("#search .dropdown").removeClass( "open" );
    } else {
      $("#search .dropdown").addClass( "open" );
      $("#search .dropdown-menu").html("<li>Loading " + val +"</li>")
      find_article( val ).done( function(results) {
        var menu = $("#search .dropdown-menu").html("");
        $.each( results, function( url, info ) {
          menu.append( "<li><a href=\"" + url + "\">" + info.title + "</a></li>");
        });
      });
    }
    console.log( "Got " + $(this).val() );
  })
});

var find_article = function( word ) {
  console.log( "Started find_article" );
  var promise = $.Deferred();

  var lookup_word = function( word, promise ) {
    console.log( "Looking up word:" + word );

    var result = {}

    // var expander = new RegExp( "(.)", "g" );
    // var matcher = new RegExp( word.replace( expander, "$1.*" ) );
    var matcher = new RegExp( word );

    $.each( window.articles.index, function( i ) {
      // console.log( "Looking at " + i );
      if( matcher.exec( i ) ) {
        // console.log( "Matched " + i );
        // console.log( window.articles.index[i] );
        $.each( window.articles.index[i], function( idx, url ) {
          // console.log( "Adding " + url );
          result[url] = window.articles.articles[url];
        } );
      }
    });

    promise.resolve( result );

    console.log( "Lookup over" );
  }

  if( window.articles === undefined ) {
    console.log( "Ajax call" );
    $.get( "/articles.index.json", {cache: true} ).done( function(data) {
      console.log( "Call done" );
      window.articles = data;
      lookup_word( word, promise )
    } );
  } else {
    console.log( "Using existing object" );
    lookup_word( word, promise );
  }

  return promise;
}