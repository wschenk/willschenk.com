@MetaDataEditor = React.createClass
  getInitialState: ->
    metadata: @props.metadata

  componentWillReceiveProps: (props) ->
    @setState metadata: props.metadata

  updateMeta: (key, event) ->
    @state.metadata[key] = event.target.value
    # @setState @state
    @props.onChange @state.metadata
    false

  render: ->
    rows = for k,v of @state.metadata
      <tr key={k}>
        <th>{k}:</th>
        <td><Input standalone type='text' name={k} value={v} onChange={this.updateMeta.bind( this, k )}/></td>
      </tr>

    <Table striped bordered condensed>
      <tbody>
        {rows}
      </tbody>
    </Table>