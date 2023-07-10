#!/bin/bash

npx @unocss/cli "site/**/*.html" -o site/main.css --watch &
npx live-server site
