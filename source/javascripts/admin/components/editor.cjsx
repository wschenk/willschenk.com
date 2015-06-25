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
      console.log @state
      @setState( @state )

  handleSave: (e) ->
    e.preventDefault()
    console.log "Pressed save"
    @state.dirty = false
    @setState @state
    console.log @state.markdown

  handleChange: (value) ->
    @state.markdown = value
    @state.dirty = true
    @restartTimer()
    @setState @state

  restartTimer: ->
    clearTimeout( @timer ) if( @timer )
    @timer = setTimeout ->
      console.log "Timeout"
      @timer = null
    , 2000

  render: ->
    # drafts = this.state.drafts.map ((item) ->
    #   <li key={item.path}>{ item.title }</li>
    # ).bind( this )

    <div className="editor">
      <EditorToolbar
        handleSave={this.handleSave}
        path={this.state.path}
        loading={this.state.loading}
        dirty={this.state.dirty}
        metadata={this.state.meta} />

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

    <div className="toolbar">
      <div className="dataeditor">
        <DataEditor onChange={this.props.metadataChange} metadata={this.props.metadata}/>
      </div>
      <div className="status">
        <p>{this.props.path}</p>
        {loading}
      </div>

      <div className="buttons">
        <ButtonToolbar>
          <Button bsStyle="success" onClick={this.props.handleSave} disabled={!this.props.dirty}>Save</Button>
        </ButtonToolbar>
      </div>
    </div>

@DataEditor = React.createClass
  updateMeta: (a,b) ->
    console.log "Clicked", a, b
    console.log @refs.form
    false

  render: ->
    rows = for k,v of @props.metadata
      updateMeta = this.updateMeta.bind( k )
      <tr key={k}>
        <th>{k}:</th>
        <td><Input standalone type='text' name={k} defaultValue={v} onChange={updateMeta}/></td>
      </tr>

    <Table striped bordered condensed>
      {rows}
    </Table>