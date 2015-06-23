@Dashboard = React.createClass
  render: ->
    <div className="dashboard">
      <div className="row">
        <MainContent />
        <Sidebar />
      </div>
    </div>

@MainContent = React.createClass
  render: ->
    <div className="maincontent">
      <DraftList/>
    </div>

@Sidebar = React.createClass
  render: ->
    <div className="sidebar">
      <PublishedList/>
    </div>

@PublishedList = React.createClass
  clickHandler: (e) ->
    e.preventDefault()
    this.props.clickHandler(this)

  getInitialState: ->
    articles: [{ title: "Hello", path: "1" }]

  componentDidMount: ->
    API.loadPublished().done (articles) =>
      console.log "Setting State", articles
      @setState( articles );

  render: ->
    articles = this.state.articles.map ((item) ->
      <li key={item.path}><a href="#">{ item.title }</a></li>
    ).bind( this )

    <ul className="nav nav-pills">
      {articles}
    </ul>