#!/usr/bin/env bash

# Ensure that we dont inadvertantly checkin any node node_modules
# or the file that we use to track our local_git repository
if [ ! -f .gitignore ]; then
  cat > .gitignore << ENDIGNORE
node_modules
.local_git
ENDIGNORE
fi

# Look inside of our post to load up where the remote
# is configured, bail if not set
REMOTE=$(awk '/^remote:/ {print $2}' index.md |head -n 1)

if [ -z "${REMOTE}" ]; then
  echo remote is not defined in the front matter of index.md
  exit 1
fi

LOCAL_GIT=$(cat .local_git 2> /dev/null)

# If blank or doest exist checkout the bare repository
if [ -z "${LOCAL_GIT}" ] || [ ! -d "${LOCAL_GIT}" ]; then
  LOCAL_GIT=$(mktemp -d)
  echo Pulling down remote git repository into $LOCAL_GIT
  git clone --bare --progress $REMOTE $LOCAL_GIT
  echo $LOCAL_GIT > .local_git
fi

git --git-dir=${LOCAL_GIT} --work-tree=. "$@"
