@DropUploader = React.createClass
  # mixins: [OverlayMixin]

  getInitialState: ->
    dragging: false
    isModalOpen: false
    progress: 30

  dragEnter: -> 
    @state.dragging = true
    @setState @state

  dragLeave: -> 
    @state.dragging = false
    @setState @state

  dropped: (e) ->
    console.log "Got drop event", e
    console.log @props.path
    console.log e.nativeEvent
    @state.isModalOpen = true
    @state.dragging = false
    @setState @state
    API.uploadFile( @props.path, e.nativeEvent, @progress ).done ->
      @state.isModalOpen = false
      @setState @state
    e.preventDefault()

  progress: (size) ->
    @state.progress = size
    @setState @state
    
  hideOverlay: ->
    @state.isModalOpen = false
    @setState @state

  render: ->
    style = {border: 'none'}
    style.border = "1px solid blue" if @state.dragging

    # uploading = <p>Uploading...</p> if @state.uploading

    <div  className="dropUploader"
          style={style}
          dropable="true" 
          onDrop={this.dropped} 
          onDragEnter={this.dragEnter} 
          onDragLeave={this.dragLeave}>
      { @renderOverlay() }
      { @props.children }
    </div>

  #   // This is called by the `OverlayMixin` when this component
  # // is mounted or updated and the return value is appended to the body.
  renderOverlay: ->
    return <span/> if !@state.isModalOpen

    <Modal title='Upload Image' onRequestHide={this.hideOverlay}>
      <div className='modal-body'>
        <ProgressBar now={@state.progress} label='%(percent)s%' />
      </div>
      <div className='modal-footer'>
        <Button onClick={this.hideOverlay}>Close</Button>
      </div>
    </Modal>
