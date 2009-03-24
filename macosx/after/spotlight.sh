#!/bin/sh

# Keep Spotlight from sucking down my CPU
PLIST=/System/Library/LaunchDaemons/com.apple.metadata.mds

if [ `id -u` -ne 0 ]; then
  echo "ERROR: please run me with sudo"
  exit 1
fi

defaults write $PLIST LowPriorityIO -bool true
defaults write $PLIST Nice -int 20
plutil -convert xml1 $PLIST.plist
