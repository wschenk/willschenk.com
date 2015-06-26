@Dashboard = React.createClass
  render: ->
    <div className="dashboard">
      <DashboardToolbar />

      <div className="row">
        <Drafts />
        <PublishedList />
      </div>
    </div>