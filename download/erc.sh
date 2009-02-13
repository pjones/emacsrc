#!/bin/sh

# Load in common items
. `dirname $0`/common.sh

URL="http://ftp.gnu.org/gnu/erc/erc-${VERSION}.tar.gz"
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  mv Makefile Makefile.org
  sed $SED_OPTIONS "s|^PREFIX *=.*|PREFIX=${PREFIX}|" < Makefile.org > Makefile
  make install > /dev/null 2>&1 || die "make failed"
)

clean_files $dir
