#!/bin/sh

. `dirname $0`/common.sh
URL="http://git.gnus.org/gnus.git"
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  git checkout $VERSION || die "bad tag: $VERSION"
  sh configure --prefix=$PREFIX --with-emacs > /dev/null 2>&1 || die "configure failed"
  make > /dev/null 2>&1 || die "make failed"
) || die "can't build gnus"

DEST=${PREFIX}/share/emacs/site-lisp/gnus
rm -rf $DEST
cp -rp $dir $DEST
