#!/bin/sh

################################################################################
#
# Fetch one or more Flickr RSS feeds and print out the URLs for the
# images in the feed.
#
################################################################################
shuffle=0

################################################################################
usage () {
  echo "Usage: flickr-rss.sh [options] url [url...]"
  echo ""
  echo "  -h This message"
  echo "  -s Shuffle the output"
}

################################################################################
set -- `getopt hs "$@"`

################################################################################
while :; do
  case "$1" in
    -h)
      usage
      exit
      ;;

    -s)
      shuffle=1
      shift
      ;;

    --)
      shift
      break
      ;;
  esac
done

################################################################################
temp_file=`mktemp -t flickr-rss.XXXX`

################################################################################
for feed; do
  xpath -o "$feed" '//entry/link[@rel="enclosure"]/@href' >> $temp_file
done

if [ $shuffle -eq 1 ]; then
  sort -R $temp_file
else
  cat $temp_file
fi

rm -f $temp_file
