#!/bin/bash

WORKDIR=/tmp/repos

mkdir -p $WORKDIR

while IFS= read -r repo
do
    PROJDIR=$(echo ${repo} | sed 's/.*\/\///g')

    mkdir -p ${WORKDIR}/${PROJDIR}
    cd ${WORKDIR}/${PROJDIR}

    if [ -d repo ]; then
        echo Updating repository ${repo}
        cd repo
        git pull origin master
    else
        echo Initial clone of ${repo}
        git clone ${repo} repo
    fi
done < repo_list
