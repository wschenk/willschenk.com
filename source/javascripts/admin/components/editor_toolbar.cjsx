@EditorToolbar = React.createClass
  getInitialState: ->
    dataEditor: false

  toggleDataeditor: ->
    @setState dataEditor: !@state.dataEditor

  renderDataEditor: ->
    return <span/> if !@state.dataEditor

    <Modal title='Metadata' onRequestHide={@toggleDataeditor}>
      <div className='modal-body'>
        <MetaDataEditor onChange={@props.onChange} metadata={@props.metadata}/>
      </div>
      <div className='modal-footer'>
        <Button onClick={this.toggleDataeditor}>Close</Button>
      </div>
    </Modal>

  render: ->
    metadata = for k,v of @props.metadata
      <MenuItem onClick={@toggleDataeditor} key={k}>
        {k}: {v}
      </MenuItem>

    publish = if @props.draft
      <NavItem onClick={@props.onPublish}>Publish Draft</NavItem>

    <AdminNavbar path={@props.path}>
      {publish}
      <NavItem disabled={!this.props.dirty} onClick={this.props.handleSave}>Save</NavItem>
      <DropdownButton title='Metadata'>
        {metadata}
      </DropdownButton>
      <NavItem disabled>{@props.path}</NavItem>
      {@renderDataEditor()}
    </AdminNavbar>
    