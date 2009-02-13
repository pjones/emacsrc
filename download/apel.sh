#!/bin/sh

# Load in common items
. `dirname $0`/common.sh

URL="http://kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/apel/apel-${VERSION}.tar.gz"
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  make install PREFIX=${PREFIX} > /dev/null 2>&1
) || die "failed to build"

clean_files $dir
