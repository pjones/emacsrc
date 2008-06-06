#!/bin/sh

. `dirname $0`/common.sh
URL="http://download.gna.org/muse-el/muse-${VERSION}.tar.gz"
file=`fetch_url $URL`
dir=`untar $file`

(
  cd $dir || die "WTF"
  sed -E "s|^PREFIX *=.*|PREFIX=${PREFIX}|" < Makefile.defs.default > Makefile.defs
  make install || die "make failed"
)

clean_files $dir
