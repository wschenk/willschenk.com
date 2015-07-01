@DashboardToolbar = React.createClass
  getInitialState: ->
    newDraft: false

  newDraft: ->
    @state.metadata = {title: "", tags: "" }
    @state.newDraft = true
    @setState @state

  closeNewDraft: ->
    @state.newDraft = false
    @setState @state

  updateMeta: (meta) ->
    @state.metadata = meta
    @setState @state

  createNewDraft: ->
    API.newDraft( @state.metadata ).then (resp) ->
      window.location = "/admin/editor?drafts=" + resp.created
    , (error) ->
      alert( error.responseJSON.error )

  render: ->
    new_draft = if @state.newDraft
      <Modal title='New Draft' onRequestHide={@closeNewDraft}>
        <div className='modal-body'>
          <MetaDataEditor metadata={@state.metadata} onChange={@updateMeta}/>
        </div>
        <div className='modal-footer'>
          <Button onClick={@createNewDraft} disabled={@state.metadata.title.length < 5}>Create Draft</Button>
        </div>
      </Modal>


    <AdminNavbar>
      {new_draft}
      <NavItem href='#' onClick={@newDraft}>New Draft</NavItem>
    </AdminNavbar>