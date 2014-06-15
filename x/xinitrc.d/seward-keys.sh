#!/bin/sh

# Mount my encrypted drives.
if [ -r ~/etc/keys.img ]; then
  mount-keys.sh &
fi
