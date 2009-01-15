#!/bin/sh

. `dirname $0`/common.sh
URL="git://github.com/technomancy/rinari.git"
DEST=${PREFIX}/share/emacs/site-lisp/rinari
dir="rinari"

if [ $VERSION = 'delete' ]; then
  rm -rf $DEST
  exit
fi

test -d $dir || git clone $URL || die "failed to clone git repo"
test -d $dir || die "git repo didn't create $dir"

(
  cd $dir
  git submodule init
  git submodule update
)

mv $dir $DEST
