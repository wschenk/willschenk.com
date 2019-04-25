#!/usr/bin/env bash

DATADIR=podcasts
mkdir -p $DATADIR

if [ -z "$(command -v jq)" ]; then
  echo Please install jq
  exit 1
fi

if [ -z "$(command -v http)" ]; then
  echo please install httpie
  exit 1
fi

function stale() {
  [ ! -f $1 ]
}

function getWorkdir() {
  if [ -z "${WORKDIR}" ]; then
    WORKDIR=$(mktemp -d)
    echo Created $WORKDIR
  fi
}

function getToken() {
  if [ ! -z "${TOKEN}" ]; then
    return
  fi

  if [ -f token.conf ]; then
    read TOKEN < token.conf
  fi

  if [ ! -z "${TOKEN}" ]; then
    return
  fi

  if ( [ -z "${POCKET_USER}" ] || [ -z "${POCKET_PASS}" ] ); then
    echo Set POCKET_USER and POCKET_PASS to login
    exit 1
  fi

  if [ -z "${TOKEN}" ]; then
    getWorkdir
    echo Getting token from the api
    cat << USER_JSON | http POST https://api.pocketcasts.com/user/login > ${WORKDIR}/login.json
{"email": "${POCKET_USER}", "password": "${POCKET_PASS}", "scope": "webplayer"}
USER_JSON

    TOKEN=$(jq -r '.token' ${WORKDIR}/login.json)
    if [ -z ${TOKEN} ]; then
      echo Unable to login
      jq -r '.message' ${WORKDIR}/login.json
      exit 1
    fi

    echo $TOKEN > token.conf
  fi
}

function authenticatedRequest() {
  getToken
  echo Loading $1 $2
  echo {} | http $1 $2 "Authorization: Bearer ${TOKEN}" > $3
}

## Actual commands

function loadStarredList() {
  authenticatedRequest POST https://api.pocketcasts.com/user/starred ${DATADIR}/starred.json
}

function loadPodcastList() {
  authenticatedRequest POST https://api.pocketcasts.com/user/podcast/list ${DATADIR}/podcast.json
}


function loadPodcastsFromStarredList() {
  getToken
  for uuid in $(jq -r '.episodes[] | .uuid' ${DATADIR}/starred.json)
  do
    outfile=${DATADIR}/episodes/${uuid}.json
    mkdir -p ${DATADIR}/episodes
    if stale $outfile; then
      echo Loading episode info for ${uuid}
      echo "{uuid: \"${uuid}\"}" | http POST https://api.pocketcasts.com/user/episode "Authorization: Bearer ${TOKEN}" > $outfile
    fi

    outfile=${DATADIR}/notes/${uuid}.json
    mkdir -p ${DATADIR}/notes
    if stale $outfile; then
      echo Loading note info for ${uuid}
      http GET https://cache.pocketcasts.com/episode/show_notes/${uuid}  "Authorization: Bearer ${TOKEN}" > $outfile
    fi
  done
}

if stale ${DATADIR}/starred.json; then
  loadStarredList
fi

if stale ${DATADIR}/podcast.json; then
  loadPodcastList
fi

loadPodcastsFromStarredList

if( [ ! -z "${WORKDIR}" ] ); then
  echo Cleaning up work directory
  rm -rf ${WORKDIR}
fi
#     | http POST https://api.pocketcasts.com/user/login > response.out
