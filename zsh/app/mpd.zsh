################################################################################
# Reads STDIN and turns it into a list of columns before sending to less.
mpd_column_list () {
  sed -r 's/^(.{12}).*$/\1/' | column | less
}

################################################################################
# Lists all artists known to MPD.
mpd_list_artists () {
  mpc list artist | sort | mpd_column_list
}

################################################################################
# List streaming stations and then play one of them.
mpd_radio () {
  zmodload zsh/mapfile
  radio_file=~/develop/pmade/rc/mpd/misc/streams
  lines=(${(f)mapfile[$radio_file]})

  typeset -A stations
  stations=()

  for line in $lines; do
    parts=(${(s:=:)line})
    name=$(echo $parts[1] | sed -r 's/\s*$//')
    url=$(echo $parts[2]  | sed -r 's/^\s*//')
    stations[$name]=$url
  done
  
  PROMPT3='station> '

  select station in ${(k)stations}; do
    if [[ -n $station ]]; then
      url=$stations[$station]
      mpc -q stop
      mpc -q clear
      mpc -q add $url
      mpc -q play
      break
    fi
  done
}

################################################################################
# Build a playlist called Random.
mpd_random () {
  keep_out_file=/tmp/mpc-genres-keep-out
  all_files_list=/tmp/mpc-all-files
  rm -f $keep_out_file $all_files_list
  touch $keep_out_file

  typeset -a exclude_genres
  exclude_genres=(
    Comedy Humor Folk Children Game
    Holiday Christmas Halloween Religious Meditation
    Audiobook Spoken Radio Introduction)

  for genre in $exclude_genres; do
    mpc search genre $genre >> $keep_out_file
  done
  
  if [ -r ~/.mpd/playlists/Ban.m3u ]; then
    cat ~/.mpd/playlists/Ban.m3u >> $keep_out_file
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
# Ban a song from showing up in my random playlist.
mpd_ban () {
  ban_file=~/.mpd/playlists/Ban.m3u
  playing_song=$(mpc current --format %file%)
  
  if [ -n "$playing_song" ]; then
    [ -r $ban_file ] || touch $ban_file
    echo $playing_song >> $ban_file
    mpc -q del 0
    echo "Banned: $playing_song"
  fi
}
