#!/bin/sh

# Load in common items
. `dirname $0`/common.sh

URL="http://keihanna.dl.sourceforge.jp/epg/29289/epg-${VERSION}.tar.gz"
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  sh configure --prefix=$PREFIX > /dev/null 2>&1 || die "configure failed"
  make > /dev/null 2>&1 || die "make failed"
  make install > /dev/null 2>&1 || die "make install failed"
)

clean_files $dir
