#!/bin/sh

cd '/Volumes/Media/Music/iTunes/iTunes Music/Morning Automation/Morning Automation'

TIME=`date +%H:%M`
DATE=`date +'%A %B %d, %Y'`

say -o 'Morning Time.aiff' "Good morning, time to wake up! It's $TIME on $DATE"
osascript ~/Develop/pmade/rc/macosx/scripts/iTunes/mobile-speakers.applescript
cat <<EOF | osascript
tell application "iTunes"
  set sound volume to 70
  play playlist "Morning Energy"
end tell
EOF
