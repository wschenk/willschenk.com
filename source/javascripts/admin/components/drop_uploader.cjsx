@DropUploader = React.createClass
  # mixins: [OverlayMixin]

  getInitialState: ->
    dragging: false
    isModalOpen: false
    progress: 30

  dragEnter: -> 
    console.log "Drag enter"
    @state.dragging = true
    @setState @state

  dragLeave: -> 
    console.log "Drag leave"
    @state.dragging = false
    @setState @state

  dragOver: (e) ->
    e.stopPropagation();
    e.preventDefault();
    e.dataTransfer.dropEffect = 'copy'; 

  dropped: (e) ->
    e.stopPropagation()
    e.preventDefault()
    console.log "Got drop event", e
    console.log @props.path
    console.log e.nativeEvent
    @state.isModalOpen = true
    @state.dragging = false
    @setState @state
    API.uploadFile( @props.path, e.nativeEvent, @progress ).then =>
      # @state.isModalOpen = false
      @setState @state
    false

  progress: (size) ->
    percent = size.position / size.totalSize * 100
    console.log "Got size", size, percent
    @state.progress = percent
    @setState @state

  hideOverlay: ->
    @state.isModalOpen = false
    @setState @state

  render: ->
    style = {background: 'inherit'}
    style.background = "rgba( 0, 0, 0, .3)" if @state.dragging

    # uploading = <p>Uploading...</p> if @state.uploading

    <div  className="dropUploader"
          style={style}
          dropable="true" 
          onDrop={this.dropped}
          onDragOver={this.dragOver}
          onDragEnter={this.dragEnter} 
          onDragLeave={this.dragLeave}>
      { @renderOverlay() }
      { @props.children }
    </div>

  #  // This is called by the `OverlayMixin` when this component
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
