################################################################################
# Shell functions for manipulating MPD playlists.

################################################################################
MPD_PLAYLIST_DIR=/var/media/Playlists

################################################################################
# Add the currently playing song to the given playlist.
mpd_move_current_to_playlist () {
  [ $# -eq 1 ] || return 1

  playlist="${MPD_PLAYLIST_DIR}/${1}.m3u"
  file=`mpc current --format %file%`
  song=`mpc current --format %title%`

  # Make sure the file exists
  [ ! -r "$playlist" ] && touch "$playlist"

  # Make sure currently playing track has a file name.
  if test -z "$file" || (echo "$file" | grep -q http://); then
    mpd_notify "Can't add stream to playlist" \
      "The currently playing track isn't a file."
    return 0
  fi

  if ! grep -Fq "$file" "$playlist"; then
    echo "$file" >> "$playlist"

    mpd_notify "Added song to playlist" \
      "Added \"$song\" to playlist \"$1\"."
  else
    mpd_notify "Song already in playlist" \
      "\"$song\" is already in the \"$1\" playlist."
  fi

  return 0
}

################################################################################
mpd_clean_for_notify () {
  echo "$1" | sed -E 's/-+/ /g'
}

################################################################################
mpd_notify () {
  summary=`mpd_clean_for_notify "$1"`
  body=`mpd_clean_for_notify "$2"`

  if which notify-send > /dev/null 2>&1; then
    notify-send "$summary" "$body"
  else
    echo "${summary}:" "$body"
  fi
}
