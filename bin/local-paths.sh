#!/bin/sh

################################################################################
if [ $# -ne 1 ]; then
    echo "Usage: local-paths.sh file-with-paths"
    exit 1
fi

################################################################################
LOCAL=`dirname $1`

################################################################################
# PATH without ~/local entries.
BASE=`echo $PATH | sed 's/:/\n/g' | grep -v $LOCAL | paste -sd:`

################################################################################
# BASE without system entries.
USER_PATH=`echo $BASE | sed 's/:/\n/g' | grep $HOME | paste -sd:`

################################################################################
# BASE without ~/ entries.
SYS_PATH=`echo $BASE | sed 's/:/\n/g' | grep -v $HOME | paste -sd:`

################################################################################
LOCAL_PATH=`sed -E "s|^|$LOCAL/|g" $1 | paste -sd:`
echo "PATH=$USER_PATH:$LOCAL_PATH:$SYS_PATH"
