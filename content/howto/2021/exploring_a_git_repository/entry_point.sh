#!/bin/bash
export REPO_DIR=${REPO_DIR:-/repository}
export WORK_DIR=${WORK_DIR:-/output}

echo REPO_DIR = $REPO_DIR
echo WORK_DIR = $WORK_DIR

if [ -d ${REPO_DIR} ]; then
   echo Using repo in ${REPO_DIR}
else
    if [ -z "$REPO" ]; then
        echo Please set the REPO env variable or mount ${REPO_DIR}
        exit 1
    fi

    git clone $REPO ${REPO_DIR}
fi

if [ ! -d ${WORK_DIR} ]; then
    echo Creating ${WORK_DIR}
    mkdir -p ${WORK_DIR}
fi


# Create a log of commits
#(cd ${REPO_DIR};git log --reverse --pretty='format:%aI|%ae|%an|%D') | sort > ${WORK_DIR}/commits.log

# Create a list of authors
#(cd ${REPO_DIR};git log --pretty=format:"%ae:%an") | sort -u > ${WORK_DIR}/authors.log

# Create a log of commits with files
(
    cd ${REPO_DIR}
    git log --pretty=format:'|%H|%ae|%an|%aI|%s' --numstat
) > ${WORK_DIR}/commits_with_files.log

# Create a list of tags
(
    cd ${REPO_DIR}
    git tag --sort=-v:refname --format='%(refname:short):%(objectname):%(*objectname):%(creatordate:iso8601-strict)'
) > ${WORK_DIR}/tags.log

cd ${WORK_DIR}

for i in /app/*rb; do
    ruby $i
done
