#!/bin/sh

# Load in common items
. `dirname $0`/common.sh

URL="http://projects.haskell.org/haskellmode-emacs/haskell-mode-${VERSION}.tar.gz"
dir=`fetch_url $URL`

DEST=${PREFIX}/share/emacs/site-lisp/haskell
rm -rf $DEST
mkdir -p $DEST
cp -r $dir/* $DEST/
clean_files $dir
