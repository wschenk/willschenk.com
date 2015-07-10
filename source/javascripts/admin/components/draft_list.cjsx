@DraftList = React.createClass
  getInitialState: ->
    drafts: []

  componentDidMount: ->
    API.loadDrafts().then (drafts) =>
      @setState( drafts );

  render: ->
    drafts = @state.drafts.map ((item) ->
      <li key={item.path}><a onClick={@props.viewPath.bind(this,item.path)}>{ item.title }</a></li>
    ).bind( this )

    <div className="maincontent">
      <h1>Drafts</h1>
      
      <ul className="nav nav-pills nav-stacked">
        {drafts}
      </ul>
    </div>