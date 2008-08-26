#!/bin/sh

. `dirname $0`/common.sh
URL="git://pmade.com/gnus"
dir="gnus-git"

test -d $dir || git clone $URL $dir || die "failed to clone git repo"
test -d $dir || die "git repo didn't create $dir"

(
  cd $dir || die "bad cd"
  sh configure && make || die "make failed"
) || die "can't build gnus"

DEST=${PREFIX}/share/emacs/site-lisp/gnus
rm -rf $DEST
mv $dir $DEST
