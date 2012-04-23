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

if [ -r ~/.ssh/agent ]; then
  . ~/.ssh/agent
  export SSH_AGENT_PID
  export SSH_AUTH_SOCK
fi

if [ -r ~/.gpg-agent-info ]; then
  . ~/.gpg-agent-info
  export GPG_AGENT_INFO
fi

for name in $daemons; do
  pkill -f daemon=$name
  zsh -c "emacs --daemon=$name" > /dev/null 2>&1
done
