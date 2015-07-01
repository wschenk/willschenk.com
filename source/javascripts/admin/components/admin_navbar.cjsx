@AdminNavbar = React.createClass
  getDefaultProps: ->
    path: ""

  getInitialState: ->
    commandModal: false

  command: (cmd) ->
    =>
      @state.commandModal = true
      @state.response = "Running " + cmd + "..."
      @setState @state
      API.runCommand( cmd ).then (data) =>
        @state.response = data
        @setState @state
      , (error) =>
        @state.response = error
        @setState @state
      
  render: ->
    <Navbar brand={<a href="/admin">Blog Admin</a>} fixedTop>
      { @response() }
      <CollapsibleNav>
        <Nav navbar>
          { @props.children }
        </Nav>
        <Nav navbar right>
          <DropdownButton title='Site Commands'>
            <NavItem onClick={@command( 'status' ) }>Git Status</NavItem>
            <NavItem onClick={@command( 'update' ) }>Update</NavItem>
            <NavItem onClick={@command( 'build' ) }>Build</NavItem>
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

