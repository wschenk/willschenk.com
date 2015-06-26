@DashboardToolbar = React.createClass
  clickHandler: (e) ->
    e.preventDefault()
    this.props.clickHandler(this)

  getInitialState: ->
    markdown: "Hello"

  XcomponentDidMount: ->
    API.loadDrafts().done (drafts) =>
      console.log "Setting State", drafts
      @setState( drafts );

  handleChange: (value) ->
    console.log "Changing value to", value
    @setState { markdown: value }

  render: ->
    <div className="toolbar">
      <ButtonToolbar>
        <Button bsStyle="primary">Refresh</Button>
        <Button bsStyle="success">Build</Button>
      </ButtonToolbar>
    </div>
