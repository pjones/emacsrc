#!/bin/sh

cd `dirname $0`
osascript fade-and-stop.applescript
osascript mobile-speakers.applescript "Computer"

cat <<EOF | osascript
tell application "iTunes"
  set volume 1
  set sound volume to 90
end tell
EOF
