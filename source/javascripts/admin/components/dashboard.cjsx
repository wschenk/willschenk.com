@Dashboard = React.createClass
  render: ->
    <div>
      <DashboardToolbar />

      <div className="dashboard">
        <div className="row">
          <Drafts />
          <PublishedList />
        </div>
      </div>
    </div>