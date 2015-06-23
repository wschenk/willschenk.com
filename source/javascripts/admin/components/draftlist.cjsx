@DraftList = React.createClass
  clickHandler: (e) ->
    e.preventDefault()
    this.props.clickHandler(this)

  getInitialState: ->
    drafts: [{ title: "Hello", path: "1" }]

  componentDidMount: ->
    API.loadDrafts().done (drafts) =>
      console.log "Setting State", drafts
      @setState( drafts );

  render: ->
    drafts = this.state.drafts.map ((item) ->
      <li key={item.path}>{ item.title }</li>
    ).bind( this )

    <ul>
      {drafts}
    </ul>