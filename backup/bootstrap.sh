#!/bin/sh

##############################################################################
die ()
{
  echo `basename $0`: $@
  exit 1
}

##############################################################################
if [ x$BACKUP_HOST = x ]; then
  echo "BACKUP_HOST not set"
  exit 1
fi

BACKUP_BASE="/Volumes/Backup/$BACKUP_HOST"
mkdir -p $BACKUP_BASE

if [ x$BACKUP_DIR != x ]; then
  mkdir -p $BACKUP_BASE/$BACKUP_DIR || die "failed to create backup dir"
else
  BACKUP_DIR=''
fi

cd $BACKUP_BASE/$BACKUP_DIR || die "failed to cd into backup dir"

##############################################################################
if [ x$SSH_AUTH_SOCK = x -o ! -r $SSH_AUTH_SOCK ]; then
  export SSH_AUTH_SOCK=`find /tmp/launch-*/Listeners -user $USER -type s|head -1`
fi

##############################################################################
SCP_OPTIONS="-pq"
RSYNC_OPTIONS="-qrulptz --delete"
