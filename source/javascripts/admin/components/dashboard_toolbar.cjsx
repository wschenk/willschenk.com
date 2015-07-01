@DashboardToolbar = React.createClass
  clickHandler: (e,v) ->
    console.log e,v 
    # e.preventDefault()
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
    <AdminNavbar>
      <NavItem href='#' onClick={this.clickHandler.bind( "refresh" )}>Refresh</NavItem>
      <NavItem href='#' onClick={this.clickHandler.bind( "build" )}>Build</NavItem>
    </AdminNavbar>