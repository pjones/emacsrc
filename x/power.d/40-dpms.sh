#!/bin/sh

################################################################################
# Reset DPMS settings.  They seem to get lost after sleeping.

################################################################################
SCRIPT=$HOME/.xinitrc.d/holmwood-dpms.sh

################################################################################
case "$1" in
  suspend)
    : # Nothing to do.
    ;;

  resume)
    [ -x $SCRIPT ] && $SCRIPT
    ;;
esac
