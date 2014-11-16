//= require 'jquery'
//= require 'bootstrap'

$( function() {
  $("#comment_shower").click( function(e) {
    e.preventDefault();
    $(".comment_universe").slideToggle();
    return false;
  } );
});