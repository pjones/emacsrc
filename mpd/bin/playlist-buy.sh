#!/bin/sh

################################################################################
# Records information about the current song, most likely playing on a
# radio station, to my "Buy" list.
. ~/lib/sh/playlists.sh

################################################################################
buy_file=~/documents/audio/music-to-buy
song=`mpc current --format '%title% - %artist% - %name%'`

if [ -n "$song" ]; then
  echo "$song" >> "$buy_file"
  mpd_notify "Added song to buy list" \
    "Added \"$song\" to the list of music to buy."
fi
