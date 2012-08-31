#!/bin/sh

################################################################################
# Make a playlist called Random.
. ~/lib/sh/playlists.sh
BAN_FILE=$MPD_PLAYLIST_DIR/Ban.m3u

################################################################################
# Build a playlist called Random.
mpd_random () {
  keep_out_file=/tmp/mpc-genres-keep-out
  all_files_list=/tmp/mpc-all-files
  rm -f $keep_out_file $all_files_list
  touch $keep_out_file

  exclude_genres="
    Comedy Humor Folk Children Game Trains
    Holiday Christmas Halloween Religious Meditation
    Audiobook Spoken Radio Introduction"

  for genre in $exclude_genres; do
    mpc search genre $genre >> $keep_out_file
  done

  if [ -r $BAN_FILE ]; then
    cat $BAN_FILE >> $keep_out_file
  fi

  sort $keep_out_file > ${keep_out_file}.sort
  mv ${keep_out_file}.sort $keep_out_file
  mpc clear -q
  mpc listall | sort > $all_files_list

  diff -u $keep_out_file $all_files_list | \
    egrep '^\+[^+]' | sed -r 's/^\+//' | \
    mpc add

  mpc rm Random
  mpc save Random
  rm -f $keep_out_file $all_files_list
}

################################################################################
mpd_random
