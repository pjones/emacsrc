#!/bin/sh

################################################################################
# Returns a heavily modified version of `mpc current`.
current_maybe_empty () {
  mpc current --format '%title% [[(%artist% / %album%)]|==%name%]' | \
    sed -re 's/==([^:-]{,20})[:-]?.*$/(\1)/' -e 's/ *\)/)/g'
}

################################################################################
# Print the current track, or a newline if nothing is playing.  This
# is necessary because `mpc current' may return nothing, not even a
# newline, when nothing is playing.
current () {
  echo `current_maybe_empty`
}

################################################################################
# Sit in a loop and print the current song every time it changes.
current_loop () {
  current

  while mpc idle player > /dev/null 2>&1; do
    current
  done
}

################################################################################
if [ $# -eq 0 -a ! -t 1 ]; then
  current_loop
else
  current
fi
