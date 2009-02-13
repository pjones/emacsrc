#!/bin/sh

. `dirname $0`/common.sh
URL="git://github.com/technomancy/rinari.git"
dir=`fetch_url $URL`

if [ $VERSION = 'delete' ]; then
  rm -rf $DEST
  exit
fi

(
  cd $dir
  git submodule init
  git submodule update
)

DEST=${PREFIX}/share/emacs/site-lisp/rinari
rm -rf $DEST
mv $dir $DEST
