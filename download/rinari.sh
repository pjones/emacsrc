#!/bin/sh

. `dirname $0`/common.sh
URL="git://github.com/pjones/rinari.git"
dir=`fetch_url $URL`

if [ $VERSION = 'delete' ]; then
  rm -rf $DEST
  exit
fi

(
  cd $dir || die "WTF"
  git submodule update --init
)

DEST=${PREFIX}/share/emacs/site-lisp/rinari
rm -rf $DEST
cp -rp $dir $DEST
