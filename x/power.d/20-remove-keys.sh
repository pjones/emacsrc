#!/bin/sh

################################################################################
# Remove ssh/gpg keys from memory when suspending.
case "$1" in
  suspend)
    # Load correct environment variables and remove keys.
    . ~/.zsh/lib/agents.zsh
    ssh-add -D
    pkill -HUP gpg-agent
    ;;

  resume)
    : # Nothing to do.
    ;;
esac
