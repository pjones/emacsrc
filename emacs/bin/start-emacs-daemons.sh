#!/bin/sh

daemons=""
[ $# -gt 0 ] && daemons="$@"

if [ $# -eq 0 ]; then
  case `hostname` in
    hawkins|seward|holmwood)
      daemons="server irc gnus"
      ;;
    *)
      daemons="server"
      ;;
  esac
fi

for name in $daemons; do
  pkill -f daemon=$name
  zsh -c "emacs --daemon=$name" > /dev/null 2>&1
done
