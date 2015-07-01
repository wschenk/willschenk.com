@PublishedList = React.createClass
  clickHandler: (e) ->
    e.preventDefault()
    this.props.clickHandler(this)

  getInitialState: ->
    articles: [{}]

  componentDidMount: ->
    API.loadPublished().then (articles) =>
      console.log "Setting State", articles
      @setState( articles );

  render: ->
    articles = this.state.articles.map ((item) ->
      <li key={item.path}><a href="/admin/editor?article=#{item.path}">{ item.title }</a></li>
    ).bind( this )

    <div className="sidebar">
      <ul className="nav nav-pills nav-stacked">
        {articles}
      </ul>
    </div>