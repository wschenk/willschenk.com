@runCommand = Reflux.createAction
  asyncResult: true

@runLater = (cmd, path) ->
  ->
    runCommand( cmd, path )

@commandResultStore = Reflux.createStore
  init: ->
    @state = @defaultState()
    @listenTo runCommand, @onRunCommand

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
