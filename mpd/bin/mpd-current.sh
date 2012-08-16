#!/bin/sh

################################################################################
# Returns a heavily modified version of `mpc current`.
mpd_current () {
  mpc current --format '%title% [[(%artist% / %album%)]|==%name%]' | \
    sed -re 's/==([^:-]{,20})[:-]?.*$/(\1)/' -e 's/ *\)/)/g'
}

################################################################################
# Sit in a loop and print the current song every time it changes.
mpd_current_loop () {
  mpd_current

  while mpc idle player > /dev/null 2>&1; do
    mpd_current
  done
}

################################################################################
if [ $# -eq 0 -a ! -t 1 ]; then
  mpd_current_loop
else
  mpd_current
fi
