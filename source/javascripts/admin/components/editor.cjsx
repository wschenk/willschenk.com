@Editor = React.createClass
  clickHandler: (e) ->
    e.preventDefault()
    this.props.clickHandler(this)

  getInitialState: ->
    article = ParseSearch( window.location.search ).article
    markdown: "Hello"
    dirty: false
    loading: true
    path: article
    meta: {}

  componentDidMount: ->
    API.loadPost( @state.path ).done (post) =>
      @state.loading = false
      @state.dirty = false
      @state.markdown = post.content
      @state.meta = post.meta
      @setState( @state )

  handleSave: (e) ->
    e.preventDefault()
    console.log "Pressed save"
    @state.saving = "Saving..."

    API.savePost( @state.path, @state.meta, @state.markdown ).done (message) =>
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
    @timer = setTimeout ->
      console.log "Timeout"
      @timer = null
    , 2000

  render: ->
    <div className="editor">
      <EditorToolbar
        handleSave={this.handleSave}
        path={this.state.path}
        loading={this.state.loading}
        saving={this.state.saving}
        dirty={this.state.dirty}
        metadata={this.state.meta}
        onChange={this.metadataChange} />

      <div className="row">
        <div className="editorPane">
          <AutosizeTextarea value={this.state.markdown} onChange={this.handleChange}/>
        </div>
        <div className="previewPane">
          <MarkdownPreview markdown={this.state.markdown} />
        </div>
      </div>
    </div>

@EditorToolbar = React.createClass
  render: ->
    loading = if @props.loading
      <p>Loading...</p>
    else
      null

    saving = if @props.saving
      <p>{@props.saving}</p>
    else
      null

    <div className="toolbar">
      <div className="dataeditor">
        <DataEditor onChange={this.props.onChange} metadata={this.props.metadata}/>
      </div>
      <div className="status">
        <p>{this.props.path}</p>
        {loading}
        {saving}
      </div>

      <div className="buttons">
        <ButtonToolbar>
          <Button bsStyle="success" onClick={this.props.handleSave} disabled={!this.props.dirty}>Save</Button>
        </ButtonToolbar>
      </div>
    </div>

@DataEditor = React.createClass
  getInitialState: ->
    metadata: {}

  componentWillReceiveProps: (props) ->
    @setState metadata: props.metadata

  updateMeta: (key, event) ->
    @state.metadata[key] = event.target.value
    # @setState @state
    @props.onChange @state.metadata
    false

  render: ->
    rows = for k,v of @state.metadata
      <tr key={k}>
        <th>{k}:</th>
        <td><Input standalone type='text' name={k} value={v} onChange={this.updateMeta.bind( this, k )}/></td>
      </tr>

    <Table striped bordered condensed>
      {rows}
    </Table>

  # render: ->
  #   rows = for k,v of @state.metadata
  #     _this = this
  #     updateMeta = (e) =>
  #       this.updateMeta( e, k )

  #     <tr key={k}>
  #       <th>{k}:</th>
  #       <td><Input standalone type='text' name={k} defaultValue={v} onChange={updateMeta}/></td>
  #     </tr>

  #   <Table striped bordered condensed>
  #     {rows}
  #   </Table>