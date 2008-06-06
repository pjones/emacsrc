#!/bin/sh

# Load in common items
. `dirname $0`/common.sh

URL="http://kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/apel/apel-${VERSION}.tar.gz"

file=`fetch_url $URL`
dir=`untar $file`

(cd $dir && make install PREFIX=${PREFIX}) || die "failed to build"
clean_files $dir
