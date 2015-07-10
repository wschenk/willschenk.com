@PublishedList = React.createClass
  getInitialState: ->
    articles: []

  componentDidMount: ->
    API.loadPublished().then (articles) =>
      @setState( articles );

  render: ->
    articles = this.state.articles.map ((item) ->
      <li key={item.path}><a onClick={@props.viewPath.bind(this,item.path)}>{ item.title }</a></li>
    ).bind( this )

    <div className="sidebar">
      <h1>Published</h1>
      <ul className="nav nav-pills nav-stacked">
        {articles}
      </ul>
    </div>