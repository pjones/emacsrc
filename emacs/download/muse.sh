#!/bin/sh

. `dirname $0`/common.sh
URL="git://pmade.com/muse.git"
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  sed $SED_OPTIONS "s|^PREFIX *=.*|PREFIX=${PREFIX}|" < Makefile.defs.default > Makefile.defs
  make clean > /dev/null 2>&1
  make install > /dev/null 2>&1 || die "make failed"
)

clean_files $dir
