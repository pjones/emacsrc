#!/bin/sh

cd '/Volumes/Media/Music/iTunes/iTunes Music/Morning Automation/Morning Automation'

long_speach ()
{
  TIME=`date +%H:%M`
  DATE=`date +'%A %B %d, %Y'`
  echo "Good morning, time to wake up! It's $TIME on $DATE." > script.txt
  echo >> script.txt
  echo "Your custom weather forecast for Lafayette Colorado." >> script.txt
  echo >> script.txt
  sh ~/Develop/pmade/rc/macosx/scripts/weather/weather.sh >> script.txt
}

short_speach ()
{
  TIME=`date +%H:%M`
  echo "The time is now $TIME" > script.txt
}

if [ x$1 = xtime ]; then
  short_speach
else
  long_speach
fi

say -f script.txt -o 'Morning Time.aiff'
osascript ~/Develop/pmade/rc/macosx/scripts/iTunes/mobile-speakers.applescript
cat <<EOF | osascript
tell application "iTunes"
  set sound volume to 70
  play playlist "Morning Energy"
end tell
EOF
