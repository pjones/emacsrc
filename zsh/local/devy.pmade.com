#!/usr/bin/env zsh

UNIVERSAL_FILE="/tmp/ssh-agent-$USER"

if [ "x$SSH_AUTH_SOCK" = "x" -o ! -S "$SSH_AUTH_SOCK" ]; then
    export SSH_AUTH_SOCK=$UNIVERSAL_FILE
elif [ ! -L $SSH_AUTH_SOCK ]; then
    ln -nfs $SSH_AUTH_SOCK $UNIVERSAL_FILE
    export SSH_AUTH_SOCK=$UNIVERSAL_FILE
fi