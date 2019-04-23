#!/bin/bash

docker run -it --rm --name cabal --network host wschenk/cabal cabal "$@"
