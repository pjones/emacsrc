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
