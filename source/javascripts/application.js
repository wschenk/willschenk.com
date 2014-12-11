//= require 'jquery'
//= require 'bootstrap-sprockets'

$( function() {
  $("#comment_shower").click( function(e) {
    e.preventDefault();
    $(".comment_universe").slideToggle();
    return false;
  } );

  if( $(".sidebar").size > 0 ) {
    $(".sidebar ul").attr( "data-offset-top", $(".sidebar").offset().top );
  }

  $.fn.originalTrigger = $.fn.trigger;
  $.fn.trigger = function(type, data) {
    console.log(typeof(type)=='string' ? type : type.type, data);
    return $(this).originalTrigger.apply(this,arguments);
  }

});