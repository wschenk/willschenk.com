@runCommand = Reflux.createAction
  asyncResult: true

@createNewDraft = Reflux.createAction
  asyncResult: true

@runLater = (cmd, path) ->
  ->
    runCommand( cmd, path )

@commandResultStore = Reflux.createStore
  init: ->
    @state = @defaultState()
    @listenTo runCommand, @onRunCommand
    @listenTo createNewDraft, @onCreateNewDraft


  defaultState: ->
    command: ""
    running: false
    result: ""

  onRunCommand: (cmd, path) ->
    console.log "Running command", cmd, path
    @state = @defaultState()
    @state.running = true
    @state.result = ""
    @state.command = cmd
    @trigger @state
    request.post "/api/" + cmd
      .query path: path
      .end (err, response) =>
        console.log response
        if response.ok
          runCommand.completed( response.body )
          @state.running = false
          @state.result = response.text
          @trigger @state
        else
          runCommand.failed( response.error )

  onCreateNewDraft: (metadata) ->
    window.LiveReload.shutDown() if window.LiveReload
    console.log "Creating new draft", metadata
    @state = @defaultState()
    @state.running = true
    @state.command = "Creating new draft"
    @trigger @state
    request.post "/api/drafts"
      .type 'form'
      .send metadata
      .end (err, response) =>
        if response.ok
          @state.running = false
          @state.result = response.text
          @state.command = ""
          @trigger @state
          viewPath response.body.created
