#!/bin/sh

if [ $# -ne 1 ]; then
  echo "Usage:   no-menubar.sh <application-name>"
  echo "Example: no-menubar.sh iTunes"
  exit 1
fi

APP="/Applications/$1.app/Contents"

if [ ! -d $APP ]; then
  echo "error: $APP doesn't seem to be a directory"
  exit 1
fi

defaults write $APP/Info LSUIPresentationMode -int 4
echo "done."
