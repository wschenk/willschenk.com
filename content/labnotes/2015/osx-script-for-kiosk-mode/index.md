---
title: OSX Script for Kiosk Mode
subtitle: make your own screen saver
date: 2015-05-20
tags:
  - osx
  - toys
  - gaze
aliases:
  - "/osx-script-for-kiosk-mode/"
  - "/articles/2015/osx-script-for-kiosk-mode/"
---

In the office, we run [Jenkins](http://jenkins-ci.org) on the same machine that we run [Benevolent Gaze](http://gaze.happyfuncorp.com). During iOS builds, the iOS simulator will take over the screen for the build, and then leave the beautiful screen on the desktop, hiding our smiling faces. We want to return to Safari in this case, but we also want to make sure that if someone is actually in front of the machine it will let them do their thing.

Here's a little script that check if the user hasn't touched the keyboard or mouse in a while, and if so it will pull Safari to the front.

```bash
#!/bin/bash
while true
do
  IDLE=$(ioreg -c IOHIDSystem |awk '/HIDIdle/ {print int($(NF)/1000000000 + 0.5)}'| sort -n |head -1)
  echo Idle $IDLE seconds

  if [ $IDLE -gt 30 ]
  then
    cat <<- EOF | osascript
      tell application "Safari"
        activate
      end tell
    EOF
  fi
  sleep 1
done
```

The first thing that this does is to call the `ioreg` command to pull data from the `IOHIDSystem`. This is what OSX uses to track _inactivity_, which is used to start powering things down to conserve battery life. The units of this measurement are _small_, so we divide by `1000000000` to get them into seconds. We round that number to the nearest integer by adding `0.5` and lopping off the right of the decimal place with `int`. Then we get the shortest number in the list using `sort | head`.

This script waits `30` seconds (`if [ $IDLE -gt 30 ]`) and then will run `osascript` to activate Safari. The here-doc between `EOF` is AppleScript which will activate _Safari_ and bring it to the front. Replace that with _Google Chrome_ if that's your preferred browser.
