//= require_tree "."

var blogAdmin = angular.module( "blogAdmin", [] );

blogAdmin.controller( 'blogCtrl', ["$scope", "$http", function( $scope, $http ) {
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
      console.log( data );
      $scope.working = data;
    })
  }

  $scope.reloadPublished();

  $scope.selectDraft = function(draft) {
    console.log( draft );
    $scope.loadRaw( draft.path );
  }

} ] );
