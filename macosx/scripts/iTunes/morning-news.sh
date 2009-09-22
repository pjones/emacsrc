#!/bin/sh

osascript ~/Develop/pmade/rc/macosx/scripts/iTunes/mobile-speakers.applescript

cat <<EOF | osascript
tell application "iTunes"
  set sound volume to 70
  play playlist "Morning News"
end tell
EOF
