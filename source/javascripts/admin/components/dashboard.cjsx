@Dashboard = React.createClass
  render: ->
    <div className="dashboard">
      <div className="row">
        <DraftList viewPath={@props.viewPath}/>
        <PublishedList viewPath={@props.viewPath}/>
      </div>
    </div>