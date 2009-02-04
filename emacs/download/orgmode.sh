#!/bin/sh

. `dirname $0`/common.sh
URL="git://repo.or.cz/org-mode.git"
dir="org-mode"

test -d $dir || git clone $URL || die "failed to clone git repo"
test -d $dir || die "git repo didn't create $dir"

(
  cd $dir || die "WTF"
  make prefix=${PREFIX} install || die "make failed"
)

clean_files $dir
