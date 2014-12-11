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

  if( $(".sidebar").size > 0 ) {
    $(".sidebar ul").attr( "data-offset-top", $(".sidebar").offset().top );
  }

  $("img[alt=\"left_float\"]").addClass( "left_float" );
  
  $.fn.originalTrigger = $.fn.trigger;
  $.fn.trigger = function(type, data) {
    console.log(typeof(type)=='string' ? type : type.type, data);
    return $(this).originalTrigger.apply(this,arguments);
  }

});