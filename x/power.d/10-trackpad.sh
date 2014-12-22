#!/bin/sh

################################################################################
# Turn the trackpad off when waking from sleep.
case "$1" in
  suspend)
    : # Nothing to do.
    ;;

  resume)
    tptoggle.sh off
    ;;
esac
