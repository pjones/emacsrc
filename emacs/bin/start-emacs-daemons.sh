#!/bin/sh

daemons=""
[ $# -gt 0 ] && daemons="$@"

if [ $# -eq 0 ]; then
  case `hostname` in
    hawkins)
      daemons="server irc gnus"
      ;;
    seward)
      daemons="server irc gnus"
      ;;
    *)
      daemons="server"
      ;;
  esac
fi

# Load SSH agent environment vars
if [ -r ~/.ssh/agent ]; then
  eval `cat ~/.ssh/agent | egrep -v ^echo`
  export SSH_AGENT_PID
  export SSH_AUTH_SOCK
fi

# Load GPG agent environment vars
if [ -r ~/.gpg-agent-info ]; then
  eval `cat ~/.gpg-agent-info`
  export GPG_AGENT_INFO
fi

for name in $daemons; do
  pkill -f daemon=$name
  zsh -c "emacs --daemon=$name" > /dev/null 2>&1
done
