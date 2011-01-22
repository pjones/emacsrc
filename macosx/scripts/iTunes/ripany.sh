#!/bin/sh

PATH=/opt/local/bin:$PATH
DEST=$HOME/Streams

usage () {
  echo "Usage: "`basename $0`" stream-name [seconds]"
  exit 1
}

if [ $# -eq 0 ]; then
  usage
fi

case $1 in
  radio1cz)
    URL="http://netshow.play.cz:8000/radio1.mp3"
    SECONDS=4200
    ;;
  -h)
    usage
    ;;
  *)
    echo "whoa, no such stream name: $1"
    exit 1
    ;;
esac

if [ $# -eq 2 ]; then
  SECONDS=$2
fi

M3U_FILE=$DEST/$1.m3u
MP3_FILE="$1-"`date +%Y%m%d-%H%M`

mkdir -p $DEST
echo "http://localhost:8000/" >> $M3U_FILE
streamripper $URL -s --quiet -d $DEST -a $MP3_FILE -l $SECONDS -o never -r
rm $M3U_FILE
