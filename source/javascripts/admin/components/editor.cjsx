@Editor = React.createClass
  clickHandler: (e) ->
    e.preventDefault()
    this.props.clickHandler(this)

  getInitialState: ->
    article = ParseSearch( window.location.search ).article
    draft = ParseSearch( window.location.search ).draft
    markdown: "Hello"
    dirty: false
    loading: true
    path: article || draft
    draft: draft
    meta: {}

  componentDidMount: ->
    API.loadPost( @state.path ).then (post) =>
      @state.loading = false
      @state.dirty = false
      @state.markdown = post.content
      @state.meta = post.meta
      @setState( @state )
    , (error) =>
      @state.error = error.error
      @setState @state

  handleSave: (e) ->
    e.preventDefault()
    console.log "Pressed save"

    @doSave()

  doSave: ->
    @state.saving = "Saving..."

    API.savePost( @state.path, @state.meta, @state.markdown ).then (message) =>
      @state.dirty = false
      @state.saving = message
      @setState @state

    @setState @state

  handleChange: (value) ->
    @state.markdown = value
    @state.dirty = true
    @restartTimer()
    @setState @state

  metadataChange: (metadata) ->
    @state.dirty = true
    @state.meta = metadata
    @restartTimer()
    @setState @state

  restartTimer: ->
    clearTimeout( @timer ) if( @timer )
    @timer = setTimeout =>
      console.log "Timeout"
      window.LiveReload.shutDown() if window.LiveReload
      @doSave()
      @timer = null
    , 2000

  render: ->
    <DropUploader path={@state.path}>
      {@errorMessage()}
      <EditorToolbar
        handleSave={@handleSave}
        path={@state.path}
        loading={@state.loading}
        saving={@state.saving}
        dirty={@state.dirty}
        metadata={@state.meta}
        onChange={@metadataChange}
        draft={@state.draft}
        onPublish={@publish}
        path={@state.path}/>
      <div className="editor">
        <div className="row">
            <div className="editorPane">
              <AutosizeTextarea value={this.state.markdown} onChange={this.handleChange}/>
            </div>
            <div className="previewPane">
              <MarkdownPreview markdown={this.state.markdown} />
            </div>
        </div>
      </div>
    </DropUploader>

  goBack: ->
    window.location = '/admin'

  errorMessage: ->
    return <span/> if !@state.error

    <Modal title='Editor Error' onRequestHide={@goBack}>
      <div className='modal-body'>
        {@state.error}
      </div>
      <div className='modal-footer'>
        <Button onClick={@goBack}>Close</Button>
      </div>
    </Modal>
