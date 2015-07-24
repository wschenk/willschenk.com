@DashboardNavbar = React.createClass
  getInitialState: ->
    newDraftModal: false
    metadata: 
      title: ""
      subtitle: ""
      tags: ""

  toggleModal: ->
    @state.newDraftModal = !@state.newDraftModal
    @state.metadata =
      title: ""
      subtitle: ""
      tags: ""

    @setState @state

  updateMeta: (metadata) ->
    @state.metadata = metadata
    @setState @state

  onCreateNewDraft: ->
    console.log "Running create new draft"
    @state.newDraftModal = false
    @setState @state
    createNewDraft( @state.metadata )

  render: ->
    <Nav navbar>
      {@newDraftModal()}
      <NavItem href='#' onClick={@toggleModal}>New Draft</NavItem>
    </Nav>

  newDraftModal: ->
    return <span/> unless @state.newDraftModal

    <Modal title='New Draft' onRequestHide={@toggleModal}>
      <div className='modal-body'>
        <MetaDataEditor metadata={@state.metadata} updateMeta={@updateMeta}/>
      </div>
      <div className='modal-footer'>
        <Button onClick={@onCreateNewDraft} disabled={@state.metadata.title.length < 5}>Create Draft</Button>
      </div>
    </Modal>
