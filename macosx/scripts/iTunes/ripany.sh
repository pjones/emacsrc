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
  radio1cz) # http://www.radio1.cz/
    URL="http://netshow.play.cz:8000/radio1.mp3"
    SECONDS=7800
    STATION="Prague Radio 1"
    SHOW="News Of Alternative Scene by DJ Josef Sedlon"
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
MP3_DIR=$1-`date +%Y%m%d-%H%M`

mkdir -p $DEST/$MP3_DIR
echo "http://localhost:8000/" >> $M3U_FILE
streamripper $URL -r -d $DEST/$MP3_DIR -a %q -l $SECONDS -o never -s
rm $M3U_FILE

COUNT=`ls $DEST/$MP3_DIR/*.mp3|wc -l`

if [ $COUNT -gt 1 ]; then
  (cd $DEST && mp3wrap $MP3_DIR.mp3 `ls $MP3_DIR/*.mp3|sort`) > /dev/null
  mv $DEST/${MP3_DIR}_MP3WRAP.mp3 $DEST/$MP3_DIR.mp3
else
  cp $DEST/$MP3_DIR/0000.mp3 $DEST/$MP3_DIR.mp3
fi

rm -r $DEST/$MP3_DIR
# TODO: add to iTunes and correctly tag items
