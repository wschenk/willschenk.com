#/bin/bash
REPO_WORK_DIR=/app/repository
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

# clocviz serves static files from its filesystem, so we need to run it from there
(cd /app/clocviz; clocviz $REPO_WORK_DIR &)

cd $(mktemp -d)
wget --recursive \
     --page-requisites \
     --convert-links \
     --no-parent \
     http://localhost:8080

 cp -r localhost:8080/* $WORK_DIR
