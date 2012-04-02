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

for name in $daemons; do
  pkill -f daemon=$name
  emacs --daemon=$name
done
