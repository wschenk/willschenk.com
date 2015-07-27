@DashboardNavbar = React.createClass
  getInitialState: ->
    newDraftModal: false
    metadata: 
      title: ""

  toggleModal: ->
    @state.newDraftModal = !@state.newDraftModal
    console.log @state.newDraftModal
    @setState @state

  render: ->
    <Nav navbar>
      {@newDraftModal()}
      <NavItem href='#' onClick={@toggleModal}>New Draft</NavItem>
    </Nav>

  newDraftModal: ->
    console.log "New Draft Modal", @state.newDraftModal
    return <span/> unless @state.newDraftModal

    <Modal title='New Draft' onRequestHide={@toggleModal}>
      <div className='modal-body'>
        <h1>This should be the meta data editor</h1>
      </div>
      <div className='modal-footer'>
        <Button onClick={@createNewDraft} disabled={@state.metadata.title.length < 5}>Create Draft</Button>
      </div>
    </Modal>
