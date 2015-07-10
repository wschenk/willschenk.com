@App = React.createClass
  getInitialState: ->
    path: null
    draft: false

  viewPath: (path) ->
    @state.path = path
    @state.draft = path.match( "^drafts/" );
    @setState @state

  render: ->
    page = if !@state.path
      <Dashboard viewPath={@viewPath}/>
    else
      <Editor path={@state.path}/>

    <div>
      <AdminNavbar path={@state.path} draft={@state.draft} viewPath={@viewPath}/>

      {page}
    </div>

