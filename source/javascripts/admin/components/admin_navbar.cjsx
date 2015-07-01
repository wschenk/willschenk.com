@AdminNavbar = React.createClass
  render: ->
    <Navbar brand='Blog Admin' fixedTop>
      <CollapsibleNav>
        <Nav navbar>
          <NavItem href='#'>Link</NavItem>
          <NavItem  href='#'>Link</NavItem>
          <DropdownButton title='Dropdown'>
            <MenuItem eventKey='1'>Action</MenuItem>
            <MenuItem eventKey='2'>Another action</MenuItem>
            <MenuItem eventKey='3'>Something else here</MenuItem>
            <MenuItem divider />
            <MenuItem eventKey='4'>Separated link</MenuItem>
          </DropdownButton>
        </Nav>
        <Nav navbar right>
          { @props.children }
        </Nav>
      </CollapsibleNav>
    </Navbar>