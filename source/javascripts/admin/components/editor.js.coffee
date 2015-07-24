@Editor = React.createClass
  mixins: [Reflux.connect(pathStore)]

  handleChange: (value) ->
    @state.markdown = value
    @state.dirty = true
    # @restartTimer()
    @setState @state

  render: ->
    <div className="editor">
      <div className="row">
        <div className="editorPane">
          <AutosizeTextarea value={@state.markdown} onChange={@handleChange}/>
        </div>
        <div className="previewPane">
          <MarkdownPreview markdown={@state.markdown} />
        </div>
      </div>
    </div>