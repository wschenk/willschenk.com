#!/bin/bash

ISBN=$1

if [ -z "$ISBN" ]; then
    echo Usage $0 ISBN
    exit 1
fi

if [ ! -f "${ISBN}.json" ]; then
    WORKFILE=$(mktemp)

    if [ ! -f "${ISBN}_search.html" ]; then
        echo "Searching isdn from bookshop"
        wget -O ${ISBN}_search.html "https://bookshop.org/books?keywords=${ISBN}"
    fi

    if [ ! -f "${ISBN}_book.html" ]; then
        echo "Looking up book info from bookshop"
        URL=$(awk '/class="cover"/ { print $3 }' ${ISBN}_search.html | sed -E 's/href="([^"]*).*/\1/')
        
        if [ -z "${URL}" ]; then
            echo Unable to find product link in search results
            exit 2
        fi

        wget -O ${ISBN}_book.html https://bookshop.org${URL}
    fi

    grep meta ${ISBN}_book.html | awk 'BEGIN {RS="<meta "} // { print } ' > ${WORKFILE}

    IMG_URL=$(grep twitter:image\" ${WORKFILE} | sed -E 's/.*content=\"([^"]*).*/\1/')
    DESCRIPTION=$(grep twitter:description ${WORKFILE} | sed -E 's/.*content=\"([^"]*).*/\1/')
    TITLE=$(grep og:title ${WORKFILE} | sed -E 's/.*content=\"([^"]*).*/\1/')

    AUTHOR=$(awk '/\(Author\)/ {print}' ${ISBN}_book.html | sed -E 's/\s*<[^>]*>//' | sed -E 's/<\/a.*//')

    echo "{\"title\": \"${TITLE}\", \"url\": \"https://bookshop.org${URL}\", \"img\": \"${IMG_URL}\", \"author\": \"${AUTHOR}\"}" | jq -r '.' > ${ISBN}.json

    if [ -f ${WORKFILE} ]; then
        rm ${WORKFILE}
    fi
fi

if [ ! -f ${ISBN}.jpg ]; then
    wget -O ${ISBN}.jpg $(jq -r '.img' ${ISBN}.json)
fi
