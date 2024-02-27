#!/bin/bash

npx unocss "site/**/*.html" -o site/main.css --watch &
npx live-server site
