#!/bin/sh

. `dirname $0`/common.sh

URL="git://github.com/eschulte/rhtml.git"
dir=`fetch_url $URL`

DEST=$PREFIX/share/emacs/site-lisp/rhtml
rm -rf $DEST
mkdir -p `dirname $DEST`
cp -rp $dir $DEST || die "failed to move rhtml to site-lisp"
