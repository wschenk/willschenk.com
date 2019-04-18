#!/bin/bash

# Download a url
function scrape_page() {
  echo Downloading $1 to $2
  http $1 Accept:application/vnd.github.v3.star+json -dh >$2 2>$3
}

function next_link_from_header() {
  # grep - Filter for the Link header
  # tr - slit each of the entries into it's own line
  # grep - look for the next link
  # sed - get rid of everything out side of the < >
  cat $1 | grep ^Link |tr ',' '\n'|grep \"next\" | sed 's/.*<\(.*\)>.*/\1/'
}

OUTDIR=$(mktemp -d)
PAGE=1
NEXTURL=https://api.github.com/users/wschenk/starred

while [ ! -z "${NEXTURL}" ]; do
  scrape_page $NEXTURL  $OUTDIR/page${PAGE}.json $OUTDIR/headers

  NEXTURL=$(next_link_from_header $OUTDIR/headers)
  PAGE=$(( $PAGE + 1 ))
done

echo Finished scraping pages

# Combine the files
# Parse them into data/stars.json

jq -s '[.[][]]' $OUTDIR/*json | \
  jq '[.[] | {name: .repo.name, full_name: .repo.full_name,
  avatar_url: .repo.owner.avatar_url, login: .repo.owner.login,
  description: .repo.description, starred_at: .starred_at,
  html_url: .repo.html_url, language: .repo.language,
  updated_at: .repo.updated_at}] ' > data/stars.json
