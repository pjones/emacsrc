#!/bin/sh

cd '/Volumes/Media/Music/iTunes/iTunes Music/Morning Automation/Morning Automation'

TIME=`date +%H:%M`
DATE=`date +'%A %B %d, %Y'`

say -o 'Morning Time.aiff' "Good morning, time to wake up! It's $TIME on $DATE"
echo 'tell application "iTunes" to play playlist "Morning Energy"' | osascript
