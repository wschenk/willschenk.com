@API = 
  loadUrl: (url) ->
    console.log "Loading", url
    unless @promises
      console.log "Creating promise"
      @promises = {}

    unless @promises[url]
      console.log "Fetching", url
      @promises[url] = $.Deferred()

      $.ajax( url ).success (data) =>
        console.log "Got back: ", data
        console.log @promises[url]
        @promises[url].resolve data

    @promises[url]

  loadPost: (path) ->
    ret = $.Deferred()

    $.ajax( '/api/post', {data: {path: path}} ).success (data) =>
      console.log "Got post back"
      ret.resolve( JSON.parse( data ) )


    ret

  loadDrafts: ->
    @loadUrl "/admin/drafts.json"

  loadPublished: ->
    @loadUrl "/admin/published.json"