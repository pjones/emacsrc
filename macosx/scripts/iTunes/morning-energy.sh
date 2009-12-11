#!/bin/sh

cd '/Volumes/Media/iTunes/Music/Morning Automation/Morning Automation'

long_speach ()
{
  TIME=`date +%H:%M`
  DATE=`date +'%A %B %d, %Y'`
  echo "Good morning, time to wake up! It's $TIME on $DATE." > script.txt
  echo >> script.txt
  echo "Your custom weather forecast for Lafayette Colorado." >> script.txt
  echo >> script.txt
  sh ~/Develop/pmade/rc/macosx/scripts/weather/weather.sh >> script.txt
  echo >> script.txt
  #echo "Shanna, if you can hear me, Peter wanted me to tell you that he loves you." >> script.txt
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
  set the_volume to (get sound volume)

  repeat while the_volume > 0
    set i to the_volume - 5
    if i < 0 then set i to 0
    set sound volume to i
    set the_volume to i
    delay 1
  end repeat

  stop
  set sound volume to 70
  play playlist "Morning Energy"
end tell
EOF
