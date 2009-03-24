#!/bin/sh

EMACS_APP=/Applications/MacPorts/Emacs.app
test $# -eq 1 && EMACS_APP=$1
sudo defaults write $EMACS_APP/Contents/Info LSUIPresentationMode -int 4
sudo plutil -convert xml1 $EMACS_APP/Contents/Info.plist
sudo chmod -go+r $EMACS_APP/Contents/Info.plist
