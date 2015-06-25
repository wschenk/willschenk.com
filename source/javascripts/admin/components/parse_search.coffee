@ParseSearch = (search) ->
  search = search.slice(1) if search.indexOf( '?' ) == 0

  ret = {}

  for assignment in search.split( "&" )
    pair = assignment.split( "=" )
    ret[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1]);

  ret
