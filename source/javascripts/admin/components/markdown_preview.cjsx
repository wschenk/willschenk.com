@MarkdownPreview = React.createClass
  render: ->
    rawMarkup = marked(this.props.markdown, {sanitize: true});

    <div className="markdown-preview" dangerouslySetInnerHTML={{__html: rawMarkup}}>
    </div>