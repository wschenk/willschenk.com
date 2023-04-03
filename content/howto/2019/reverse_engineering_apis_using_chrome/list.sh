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

function unrollPodcasts() {
  mkdir -p ${DATADIR}/podcasts
  for podcastUuid in $(jq -r '.podcasts[] | .uuid' ${DATADIR}/podcast.json)
  do
    jq -r ".podcasts[] | select( .uuid == \"${podcastUuid}\" ) | ." podcasts/podcast.json > ${DATADIR}/podcasts/${podcastUuid}.json
  done
}

function loadPodcastsFromStarredList() {
  getToken
  for uuid in $(jq -r '.episodes[] | .uuid' ${DATADIR}/starred.json)
  do
    outfile=${DATADIR}/episodes/${uuid}.json
    mkdir -p ${DATADIR}/episodes
    if stale $outfile; then
      echo ${uuid} episode info
      echo "{uuid: \"${uuid}\"}" | http POST https://api.pocketcasts.com/user/episode "Authorization: Bearer ${TOKEN}" > $outfile
    fi

    outfile=${DATADIR}/notes/${uuid}.json
    mkdir -p ${DATADIR}/notes
    if stale $outfile; then
      echo ${uuid} note info
      http GET https://cache.pocketcasts.com/episode/show_notes/${uuid}  "Authorization: Bearer ${TOKEN}" > $outfile
    fi
  done
}

function combinePodcastAndEpisodeInfo() {
  getWorkdir

  OUTDIR=${DATADIR}/merged
  mkdir -p $OUTDIR

  > ${WORKDIR}/combined_summary.json

  for uuid in $(jq -r '.episodes[] | .uuid' ${DATADIR}/starred.json)
  do
    echo Merging ${uuid}
    jq '{episode: .}' ${DATADIR}/episodes/${uuid}.json > $WORKDIR/combined.json

    podcastUuid=$(jq -r '.podcastUuid' ${DATADIR}/episodes/${uuid}.json)

    jq '{podcast: .}' ${DATADIR}/podcasts/${podcastUuid}.json >> $WORKDIR/combined.json
    jq '{note: .}' ${DATADIR}/notes/${uuid}.json >> $WORKDIR/combined.json

    jq -s 'add | {
      episodeTitle: .episode.title,
      audioUrl: .episode.url,
      published: .episode.published,
      duration: .episode.duration,
      size: .episode.size,
      podcastTitle: .podcast.title,
      author: .podcast.author,
      description: .podcast.description,
      podcastUrl: .podcast.url,
      notes: .note.show_notes }' $WORKDIR/combined.json > $OUTDIR/${uuid}.json
    cat ${OUTDIR}/${uuid}.json >> ${WORKDIR}/combined_summary.json
  done

  # Turn it into an array
  jq -s '. | .' ${WORKDIR}/combined_summary.json > ${DATADIR}/starred_combined.json
}

if stale ${DATADIR}/starred.json; then
  loadStarredList
  loadPodcastsFromStarredList
fi

if stale ${DATADIR}/podcast.json; then
  loadPodcastList
  unrollPodcasts
fi

combinePodcastAndEpisodeInfo

if( [ ! -z "${WORKDIR}" ] ); then
  echo Cleaning up work directory
  rm -rf ${WORKDIR}
fi
#     | http POST https://api.pocketcasts.com/user/login > response.out
