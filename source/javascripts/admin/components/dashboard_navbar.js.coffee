@DashboardNavbar = React.createClass
  getInitialState: ->
    newDraftModal: false
    metadata: 
      title: ""

  toggleModal: ->
    @state.newDraftModal = !@state.newDraftModal
    @state.metadata =
      title: ""
      subtitle: ""
      tags: ""
    @setState @state

  render: ->
    <Nav navbar>
      {@newDraftModal()}
      <NavItem href='#' onClick={@toggleModal}>New Draft</NavItem>
    </Nav>

  closeModal: ->
    @state.newDraftModal = false
    @setState @state

  updateMeta: (metadata) ->
    @state.metadata = metadata
    @setState @state

  newDraftModal: ->
    console.log "New Draft Modal", @state.newDraftModal
    return <span/> unless @state.newDraftModal

    <Modal title='New Draft' onRequestHide={@toggleModal}>
      <div className='modal-body'>
        <MetadataEditor metadata={@state.metadata} onChange={@updateMeta}/>
      </div>
      <div className='modal-footer'>
        <Button onClick={@closeModal}>Cancel</Button>
        <Button bsStyle='primary' onClick={@createNewDraft} disabled={@state.metadata.title.length < 5}>Update Data</Button>
      </div>
    </Modal>
