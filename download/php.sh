#!/bin/sh

# Load in common items
. `dirname $0`/common.sh

URL="https://github.com/ejmr/php-mode.git"
DEST=${PREFIX}/share/emacs/site-lisp/
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  git checkout $VERSION || die "bad tag: $VERSION"
  cp $dir/*.el $DEST/
)

clean_files $dir
