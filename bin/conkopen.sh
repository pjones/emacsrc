#!/bin/sh

# Open and display Conkeror
osascript -e 'tell application "Conkeror" to activate'

# Open the given URL in Conkeror
nc 127.0.0.1 4242 > /dev/null <<EOF
conkeror.url_remoting_fn('$1');
repl.quit();
EOF
