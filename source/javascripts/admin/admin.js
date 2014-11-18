//= require_tree "."

var blogAdmin = angular.module( "blogAdmin", ['autoGrow'] );

blogAdmin.controller( 'blogCtrl', ["$scope", "$http", function( $scope, $http ) {
  $scope.metadata = ""
  $scope.article = "";
  $scope.active = 'dashboard'

  $scope.reloadDrafts = function() {
    $http.get( "/admin/drafts.json" ).success( function( data ) {
      $scope.drafts = data;
    });    
  }
  $scope.reloadDrafts();

  $scope.reloadPublished = function() {
    $http.get( "/admin/published.json" ).success( function( data ) {
      $scope.published = data;
    });
  }

  $scope.loadRaw = function( path ) {
    $http.get( "/api/raw", {params: {file: path} }).success( function( data ) {
      $scope.parseRaw( data );
      $scope.active = "editor";
    })
  }

  $scope.parseRaw = function( raw ) {
    $scope.metadata = raw.match( /^---(.|\s)*?---/m )[0];
    $scope.article = raw.substr( $scope.metadata.length )
  }

  $scope.reloadPublished();

  $scope.selectDraft = function(draft) {
    console.log( draft );
    $scope.loadRaw( draft.path );
  }
} ] );


// From https://gist.github.com/D-system/5480897
angular.module('autoGrow', []).directive('autoGrow', function() {
    return function(scope, element, attr) {
        var minHeight, paddingLeft, paddingRight, $shadow = null;
 
        function createShadow(){
 
            minHeight = element[0].offsetHeight;
            if (minHeight === 0)
                return ;
            paddingLeft = element.css('paddingLeft');
            paddingRight = element.css('paddingRight');
 
            $shadow = angular.element('<div></div>').css({
                position: 'absolute',
                top: -10000,
                left: -10000,
                width: element[0].offsetWidth - parseInt(paddingLeft ? paddingLeft : 0, 10) - parseInt(paddingRight ? paddingRight : 0, 10),
                fontSize: element.css('fontSize'),
                fontFamily: element.css('fontFamily'),
                lineHeight: element.css('lineHeight'),
                resize: 'none'
            });
            angular.element(document.body).append($shadow);
 
        }
 
        var update = function() {
            if ($shadow === null)
                createShadow();
            if ($shadow === null)
                return ;
            var times = function(string, number) {
                for (var i = 0, r = ''; i < number; i++) {
                    r += string;
                }
                return r;
            };
 
            var val = element.val().replace(/</g, '&lt;')
                .replace(/>/g, '&gt;')
                .replace(/&/g, '&amp;')
                .replace(/\n$/, '<br/>&nbsp;')
                .replace(/\n/g, '<br/>')
                .replace(/\s{2,}/g, function(space) { return times('&nbsp;', space.length - 1) + ' '; });
            $shadow.html(val);
 
            element.css('height', Math.max($shadow[0].offsetHeight + 130, minHeight) + 'px');
        };
 
        element.bind('keyup keydown keypress change focus', update);
        scope.$watch(attr.ngModel, update);
        scope.$watch(function () { return element.is(':visible') ; }, update);
    };
});