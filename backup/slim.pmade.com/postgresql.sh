#!/bin/sh

BACKUP_HOST=slim.pmade.com
BACKUP_DIR=postgresql
. `dirname $0`/../bootstrap.sh
scp $SCP_OPTIONS pmade.com:/opt/backup/postgresql/\*-latest . || die "scp failed"
