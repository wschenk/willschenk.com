@viewPath = Reflux.createAction
  asyncResult: true

@pathStore = Reflux.createStore
  init: ->
    @state = @defaultState()
    @listenTo viewPath, @onViewPath

  defaultState: ->
    loading: false
    dirty: false
    path: null
    metadata: {}
    markdown: ""

  onViewPath: (path) ->
    console.log "Loading path", path
    @state = @defaultState()
    @state.path = path
    @state.loading = true
    @trigger @state
    request.get "/api/post"
      .query path: path
      .end (err, response) =>
        console.log response
        if response.ok
          viewPath.completed( response.body )
          d = response.body
          @state.loading = false
          @state.metadata = d.meta
          @state.markdown = d.content
          @trigger @state
        else
          viewPath.failed( response.error )
