#!/bin/sh

# Load in common items
. `dirname $0`/common.sh

URL="http://orgmode.org/org-${VERSION}.tar.gz"

file=`fetch_url $URL`
dir=`untar $file`

(
  cd $dir || die "WTF"
  mv Makefile Makefile.org
  sed -E "s|^prefix=.*|prefix=${PREFIX}|" < Makefile.org > Makefile
  make install || die "make failed"
)

clean_files $dir
