#!/bin/sh

SECONDS=600
URL=http://stream.kgnu.net:8000/KGNU_live_med.mp3.m3u

if [ $# -ge 1 ]; then
  SECONDS=$1
  shift
fi

cd '/Volumes/Media/Music/iTunes/iTunes Music/KGNU/KGNU Morning Stream Rip'
rm -f KGNU.mp3

/opt/local/bin/streamripper $URL \
  -s --quiet -a KGNU -l $SECONDS -o always

rm -rf KGNU.cue incomplete

# now play the stream we just ripped
osascript ~/Develop/pmade/rc/macosx/scripts/iTunes/mobile-speakers.applescript

cat <<EOF | osascript
tell application "iTunes"
  set sound volume to 70
  play playlist "Morning News"
end tell
EOF
