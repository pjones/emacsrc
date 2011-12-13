#!/bin/sh

. `dirname $0`/common.sh
URL="git://repo.or.cz/org-mode.git"
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  git checkout $VERSION || die "bad tag: $VERSION"
  make clean > /dev/null 2>&1
  make prefix=${PREFIX} install > /dev/null 2>&1 || die "make failed"
)

clean_files $dir
