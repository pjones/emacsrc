#!/bin/sh

if tmux list-sessions|awk -F: '{print $1}'|egrep -q '^clocks$'; then
  exec tmux at -t clocks
fi

tmux \
  new-session -s clocks 'tty-clock -c -C 4 -f "Colorado %Y-%m-%d"' \; \
  split-window -h 'env TZ=America/New_York tty-clock -c -f "New York %Y-%m-%d"' \; \
  split-window -v -t 0 'env TZ=Europe/Rome tty-clock -c -f "Italy %Y-%m-%d"' \; \
  split-window -v -t 1 'env TZ=UTC tty-clock -c -f "UTC %Y-%m-%d"' \; \
  set status off
