#!/bin/sh

cd '/Volumes/AV RAID/iTunes/Music/Morning Automation/Morning Automation'
AUX_FILE=/tmp/speech-script.txt

long_speach ()
{
  TIME=`date +%k:%M`
  DATE=`date +'%A %B %e, %Y'`
  echo "Good morning, time to wake up! It's $TIME on $DATE." > script.txt
  echo >> script.txt
  echo "Your custom weather forecast for Lafayette Colorado." >> script.txt
  echo >> script.txt
  sh ~/Develop/pmade/rc/macosx/scripts/weather/weather.sh >> script.txt
  echo >> script.txt
  
  if [ -r $AUX_FILE ]; then
    cat $AUX_FILE >> script.txt
    cat /dev/null > $AUX_FILE
  fi
}

short_speach ()
{
  TIME=`date +%k:%M`
  echo "The time is now $TIME" > script.txt
}

if [ x$1 = xtime ]; then
  short_speach
else
  long_speach
fi

say -f script.txt -o 'Morning Time.aiff'
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
