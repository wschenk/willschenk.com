#!/bin/bash

docker run -it --rm --name cabal --network host $USER/cabal cabal "$@"
