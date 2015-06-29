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

  savePost: (path, meta, body) ->
    ret = $.Deferred()

    $.ajax( '/api/post', {method: 'POST', data: {path: path, meta: meta, body: body} } ).success (data) =>
      console.log "Saved post"
      ret.resolve data

    ret

  uploadFile: ( path, nativeEvent, process_cb ) ->
    console.log "Uploading image to path"

    fd = new FormData()
    fd.append 'path', path
    fd.append 'file', nativeEvent.dataTransfer.files[0]

    $.ajax 
      type: "post"
      url: '/api/images'
      xhr: ->
        xhr = new XMLHttpRequest()
        xhr.upload.onprogress = process_cb
        xhr
      cache: false
      contentType: false
      # complete: uploadCompleted
      processData: false
      data: fd


  loadDrafts: ->
    @loadUrl "/admin/drafts.json"

  loadPublished: ->
    @loadUrl "/admin/published.json"