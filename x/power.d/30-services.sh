#!/bin/sh

################################################################################
# Stop some running services like IRC and mail when suspending.
case "$1" in
  suspend)
    ~/bin/stop-services.sh
    ;;

  resume)
    : # Nothing to do.
    ;;
esac
