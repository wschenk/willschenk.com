#!/bin/bash
REPO_WORK_DIR=/repository
WORK_DIR=/output

if [ -d ${REPO_WORK_DIR} ]; then
   echo Using repo in ${REPO_WORK_DIR}
else
    if [ -z "$REPO" ]; then
        echo Please set the REPO env variable or mount ${REPO_WORK_DIR}
        exit 1
    fi

    git clone $REPO ${REPO_WORK_DIR}
fi

if [ ! -d ${WORK_DIR} ]; then
    echo Creating ${WORK_DIR}
    mkdir -p ${WORK_DIR}
fi


# Create a log of commits
(cd ${REPO_WORK_DIR};git log --reverse --pretty='format:%aI|%ae|%an|%D') | sort > ${WORK_DIR}/commits.log

# Create a list of authors
(cd ${REPO_WORK_DIR};git log --pretty=format:"%ae:%an") | sort -u > ${WORK_DIR}/authors.log

# Create a log of commits with files
(cd ${REPO_WORK_DIR};git log --pretty=format:'|%h|%ae|%an|%aI|%s' --numstat) > ${WORK_DIR}/commits_with_files.log

cd ${WORK_DIR}

for i in /app/*rb; do
    ruby $i
done
