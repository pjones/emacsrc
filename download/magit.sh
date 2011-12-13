#!/bin/sh

. `dirname $0`/common.sh
URL="git://github.com/magit/magit.git"
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  git checkout $VERSION || die "bad tag: $VERSION"
  sed "s|^PREFIX=.*|PREFIX=${PREFIX}|" < Makefile > Makefile.fix
  make -f Makefile.fix install || die "make install failed"
  rm Makefile.fix
)

clean_files $dir
