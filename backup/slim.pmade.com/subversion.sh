#!/bin/sh

BACKUP_HOST=slim.pmade.com
BACKUP_DIR=subversion
. `dirname $0`/../bootstrap.sh
rsync $RSYNC_OPTIONS pmade.com:/opt/backup/subversion/ . || die "rsync failed"
