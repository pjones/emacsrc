#!/bin/sh

################################################################################
# Lock the screen when suspending.

################################################################################
lock_screen () {
  image="~/.lock-screen.png"
  opts=""

  if [ -r $image ]; then
    opts="${opts} -i ${image}"
  fi

  i3lock -c 444444 $opts
}

################################################################################
case "$1" in
  suspend)
    pgrep -u `id -u` i3lock || lock_screen
    ;;

  resume)
    : # Nothing to do.
    ;;
esac
