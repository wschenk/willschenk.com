#!/usr/bin/env bash

if [ ! $(command -v jq) ]; then
  echo This command requires jq to be installed
  exit 1
fi

if [ ! $(command -v curl) ]; then
  echo This command requires curl to be installed
  exit 1
fi

if [ ! $(command -v lpass) ]; then
  echo This command requires lpass to be installed
  exit 1
fi

SCRIPT_PATH="${BASH_SOURCE}"
while [ -L "${SCRIPT_PATH}" ]; do
  SCRIPT_DIR="$(cd -P "$(dirname "${SCRIPT_PATH}")" >/dev/null 2>&1 && pwd)"
  SCRIPT_PATH="$(readlink "${SCRIPT_PATH}")"
  [[ ${SCRIPT_PATH} != /* ]] && SCRIPT_PATH="${SCRIPT_DIR}/${SCRIPT_PATH}"
done
SCRIPT_PATH="$(readlink -f "${SCRIPT_PATH}")"
SCRIPT_DIR="$(cd -P "$(dirname -- "${SCRIPT_PATH}")" >/dev/null 2>&1 && pwd)"


# Load token if conf file is present
if [ -z "${POCKET_TOKEN}" ]; then
  if [ -f "${SCRIPT_PATH}/pocket_token.conf" ]; then
    read POCKET_TOKEN < ${SCRIPT_DIR}/../pocket_token.conf
  fi
fi

# Attempt authentication if no token but user/pass is set
if [ -z "${POCKET_TOKEN}" ]; then
  if ! lpass status; then
    lpass login wschenk@gmail.com
  fi

  POCKET_USER=`lpass show –password pocketcasts.com |awk '/Username/ { print $2}'`
  POCKET_PASS=`lpass show –password pocketcasts.com |awk '/Password/ { print $2}'`
  
  if ( [ -z "${POCKET_USER}" ] || [ -z "${POCKET_PASS}" ] ); then
    echo Set POCKET_USER and POCKET_PASS to login
    exit 1
  fi

  echo Logging in ${POCKET_USER}
  LOGIN_JSON="{'email': '${POCKET_USER}', 'password': '${POCKET_PASS}', 'scope': 'webplayer'}"
  curl -d "${LOGIN_JSON}" -H "Content-Type: application/json" -X POST \
    https://api.pocketcasts.com/user/login | jq -r .token > ${SCRIPT_DIR}/../pocket_token.conf

  read POCKET_TOKEN < ${SCRIPT_DIR}/../pocket_token.conf
fi

if [ -z "${POCKET_TOKEN}" ]; then
  echo Unable to log in
  exit 1
fi

WORKDIR=$(mktemp -d)
echo Working in ${WORKDIR}

echo podcasts.json
curl -s -d "{v:1}" -X POST \
    -H "Authorization: Bearer ${POCKET_TOKEN}" \
    -H "Content-Type: application/json"  \
    https://api.pocketcasts.com/user/podcast/list \
    > ${WORKDIR}/podcasts.json

echo starred.json
curl -s -d "{}" -X POST \
  -H "Authorization: Bearer ${POCKET_TOKEN}" \
  -H "Content-Type: application/json"  \
  https://api.pocketcasts.com/user/starred > ${WORKDIR}/starred.json

echo

for uuid in $(jq -r '.episodes[] | .uuid' ${WORKDIR}/starred.json); do
  mkdir -p ${WORKDIR}/episodes/${uuid}

  jq -r ".episodes[] | select( .uuid == \"${uuid}\" ) | ." ${WORKDIR}/starred.json > ${WORKDIR}/episodes/${uuid}/info.json

  echo ${uuid} Notes
  curl -s \
    -H "Authorization: Bearer ${POCKET_TOKEN}" \
    -H "Content-Type: application/json"  \
    https://cache.pocketcasts.com/episode/show_notes/${uuid} \
    > ${WORKDIR}/episodes/${uuid}/notes.json
done

echo

mkdir ${WORKDIR}/podcasts
for podcastUuid in $(jq -r '.podcasts[] | .uuid' ${WORKDIR}/podcasts.json)
do
  echo $podcastUuid Show info
  jq -r ".podcasts[] | select( .uuid == \"${podcastUuid}\" ) | ." ${WORKDIR}/podcasts.json > ${WORKDIR}/podcasts/${podcastUuid}.json
done

echo

> ${WORKDIR}/combined_work.json
for episodeUuid in $(jq -r '.episodes[] | .uuid' ${WORKDIR}/starred.json); do
  dir=${WORKDIR}/episodes/${episodeUuid}
  podcastUuid=$(jq -r '.podcastUuid' ${dir}/info.json)

  jq '{episode: .}' ${dir}/info.json > ${dir}/work.json
  jq '{note: .}' ${dir}/notes.json >> ${dir}/work.json
  jq '{podcast: .}' ${WORKDIR}/podcasts/${podcastUuid}.json >> ${dir}/work.json

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
    notes: .note.show_notes }' ${dir}/work.json >> ${WORKDIR}/combined_work.json
done

echo
echo Writing starred_combined.json
jq -s '. | .' ${WORKDIR}/combined_work.json > ${SCRIPT_DIR}/../data/starred_podcasts.json
