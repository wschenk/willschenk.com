@DraftList = React.createClass
  clickHandler: (e) ->
    e.preventDefault()
    this.props.clickHandler(this)

  getInitialState: ->
    drafts: []

  componentDidMount: ->
    API.loadDrafts().done (drafts) =>
      console.log "Setting State", drafts
      @setState( drafts );

  render: ->
    drafts = this.state.drafts.map ((item) ->
      <li key={item.path}><a href="/admin/editor?article=#{item.path}">{ item.title }</a></li>
    ).bind( this )

    <ul className="nav nav-pills nav-stacked">
      {drafts}
    </ul>
  