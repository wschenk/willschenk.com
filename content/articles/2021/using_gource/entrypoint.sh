#!/bin/bash

# Stop on first error
set -e

# Define defaults
RES="${RES:-1920x1080}"
DEPTH="${DEPTH:-24}"
FRAMES="${FRAMES:-60}"
SEC_PER_DAY=${SEC_PER_DAY:-0.3}
HIDE=${HIDE:-filenames}
COMPRESSION=${COMPRESSION:-18}
GOURCE_ARGS=${GOURCE_ARGS:-}

# Internal directories
WORK_DIR=/workspace && mkdir -p ${WORK_DIR}
AVATAR_DIR=/avatars && mkdir -p ${AVATAR_DIR}

# Download the repository
if [ -z "$REPO" ]; then
  echo REPO is not set!
  exit 1
fi

mkdir -p /repositories
if [ ! -d /repositories/repo ]; then
    echo Cloning $REPO
    git clone $REPO /repositories/repo
else
    echo Updating $REPO
    (cd /repositories/repo; git pull)
fi

# Pull down the authors from the repositories
for i in /repositories/*
do
    (cd ${i};git log --pretty=format:"%ae:%an")
done | sort -u > ${WORK_DIR}/authors

while IFS= read -r line; do
    readarray -d : -t strarr <<< "$line"
    email=${strarr[0]}
    name=${strarr[1]::-1}
    #echo "$name $email"

    filename=${AVATAR_DIR}/${name}.png
    #echo Looking for ${filename}
    if [ ! -f "${filename}" ]; then
        md5=$(echo -n "$email" | tr '[A-Z]' '[a-z]' | md5sum | cut -d" " -f1)
        curl http://www.gravatar.com/avatar/$md5?size=512 -o "${filename}"
    else
        echo ${filename} already exists
    fi

done < ${WORK_DIR}/authors

# Find the tags in the repo and sort by semver
cd /repositories/repo
> ${WORK_DIR}/slices

first_commit=$(git rev-list --max-parents=0 HEAD | head -1)
git tag --sort=v:refname > ${WORK_DIR}/tags

commit_time() {
    t=$(git log --pretty='%aI' -n 1 $1 | tr 'T' ' ')
    echo ${t::-6}
}

start_time=$(commit_time $first_commit)

while IFS= read -r line; do
    time=$(commit_time $line)
    if [ "${start_time}" != "${time}" ]; then
        echo ${line},${start_time},${time} >> ${WORK_DIR}/slices
    else
        echo skipping ${line}
    fi
    start_time=${time}
done < ${WORK_DIR}/tags

readarray linearray < ${WORK_DIR}/slices

for line in "${linearray[@]}"
do
    echo $line
    readarray -d , -t strarr <<< "$line"
    name=${strarr[0]}
    starttime=${strarr[1]}
    stoptime=${strarr[2]::-1}
    echo ${name} -- ${starttime} -- ${stoptime}

    if [ ! -f "${WORK_DIR}/${name}.ppm" ];then
        echo Generating ${name}.ppm
        xvfb-run -a -s "-screen 0 ${RES}x${DEPTH}" gource $RES \
                 -o ${WORK_DIR}/${name}.ppm \
                 -r 60 --auto-skip-seconds 1 --max-file-lag 0.1 \
                 --stop-at-end "-$RES" --user-image-dir ${AVATAR_DIR} \
                 --highlight-all-users -s 0.5 \
                 --start-date "${starttime}" \
                 --stop-date "${stoptime}" \
                 --seconds-per-day ${SEC_PER_DAY} --hide ${HIDE} /repositories/repo
    else
        echo ${name}.ppm already exists
    fi

    if [ ! -f "${WORK_DIR}/${name}.webm" ]; then
        if [ -s "${WORK_DIR}/${name}.ppm" ]; then
            ffmpeg -y -r 60 \
                   -f image2pipe \
                   -vcodec ppm -i ${WORK_DIR}/${name}.ppm \
                   -vcodec libvpx \
                   -b 10000K ${WORK_DIR}/${name}.webm
        else
            echo ${name}.ppm is empty
        fi
    else
        echo ${name}.webm already exists
    fi

    if [ -f "${WORK_DIR}/${name}.webm" ]; then
        echo Copying to ${name}.webm to output directory
        cp ${WORK_DIR}/${name}.webm /output
    fi
done
