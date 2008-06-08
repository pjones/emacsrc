#!/bin/sh

SERVER=""

if [ "x$SCREEN_SESSION_NAME" != "x" -a -r "/tmp/emacs$UID/$SCREEN_SESSION_NAME" ]; then
    SERVER="-s $SCREEN_SESSION_NAME"
    screen -r -X select 0
elif [[ $OSNAME == "Darwin" ]]; then
    osascript -e 'tell application "Emacs" to activate'
fi

emacsclient $SERVER -a 'emacs' "$@"
