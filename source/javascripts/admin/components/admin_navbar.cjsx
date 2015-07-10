@AdminNavbar = React.createClass
  getDefaultProps: ->
    path: ""
    draft: false

  getInitialState: ->
    commandModal: false

  command: (cmd, path) ->
    =>
      path = @props.path if path
      @state.commandModal = true
      @state.response = "Running " + cmd + "..."
      @setState @state
      API.runCommand( cmd, path ).then (data) =>
        @state.response = data
        console.log "Response", data
        @setState @state
      , (error) =>
        @state.response = error
        @setState @state
      
  render: ->
    subnav = unless @state.draft
      <DashboardToolbar/>
    else
      <EditorToolbar />

    <Navbar brand={<a href="/admin">Blog Admin</a>} fixedTop>
      { @response() }
      <CollapsibleNav>
        {subnav}
        <Nav navbar right>
          <DropdownButton title='Site Commands'>
            <NavItem onClick={@command( 'diff', true )}>Diff</NavItem>
            <NavItem onClick={@command( 'status' ) }>Git Status</NavItem>
            <NavItem onClick={@command( 'update' ) }>Update</NavItem>
            <NavItem onClick={@command( 'build' ) }>Build</NavItem>
            <NavItem onClick={@command( 'deploy' )}>Deploy</NavItem>
          </DropdownButton>
          <NavItem href={"/" + @props.path}>Preview</NavItem>
        </Nav>
      </CollapsibleNav>
    </Navbar>

  closeResponse: ->
    @state.response = null
    @setState @state

  response: ->
    return <span/> if !@state.response

    <Modal title='Command Response' onRequestHide={@closeResponse} bsSize='large'>
      <div className='modal-body'>
        <pre>{@state.response}</pre>
      </div>
      <div className='modal-footer'>
        <Button onClick={@closeResponse}>Close</Button>
      </div>
    </Modal>

