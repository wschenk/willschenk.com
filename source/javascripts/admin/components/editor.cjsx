@Editor = React.createClass
  clickHandler: (e) ->
    e.preventDefault()
    this.props.clickHandler(this)

  getInitialState: ->
    markdown: "Hello"

  XcomponentDidMount: ->
    API.loadDrafts().done (drafts) =>
      console.log "Setting State", drafts
      @setState( drafts );

  handleChange: (value) ->
    console.log "Changing value to", value
    @setState { markdown: value }

  render: ->
    # drafts = this.state.drafts.map ((item) ->
    #   <li key={item.path}>{ item.title }</li>
    # ).bind( this )

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

