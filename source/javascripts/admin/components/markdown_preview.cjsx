@MarkdownPreview = React.createClass
  render: ->
    console.log "MarkdownPreview"
    rawMarkup = marked(this.props.markdown, {sanitize: true});

    <div className="markdown-preview" dangerouslySetInnerHTML={{__html: rawMarkup}}>
    </div>