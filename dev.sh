#!/bin/bash

docker run --rm -it -v $(pwd):/src -p 1313:1313 klakegg/hugo:0.69.0 server --bind 0.0.0.0 -D --watch --cleanDestinationDir --disableFastRender
