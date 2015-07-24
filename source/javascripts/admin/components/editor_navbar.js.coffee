@EditorNavbar = React.createClass
  mixins: [Reflux.connect(pathStore)]

  getInitialState: ->
    metadata: {}

  toggleModal: ->
    @state.newDraftModal = !@state.newDraftModal
    console.log @state.newDraftModal
    @setState @state

  render: ->
    metadata = for k,v of @state.metadata
      <MenuItem onClick={@toggleDataeditor} key={k}>
        {k}: {v}
      </MenuItem>

    <Nav navbar>
      {@newDraftModal()}
      <NavItem href='#' onClick={@toggleModal}>Save</NavItem>
      <DropdownButton title='Metadata'>
        {metadata}
      </DropdownButton>

      <NavItem disabled>{@state.path}</NavItem>
    </Nav>

  newDraftModal: ->
    return <span/> unless @state.newDraftModal

    <Modal title='New Draft' onRequestHide={@toggleModal}>
      <div className='modal-body'>
        <h1>This should be the meta data editor</h1>
      </div>
      <div className='modal-footer'>
        <Button onClick={@createNewDraft} disabled={@state.metadata.title.length < 5}>Create Draft</Button>
      </div>
    </Modal>
