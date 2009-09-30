#!/bin/sh

# Load in common items
. `dirname $0`/common.sh

URL="http://ftp.gnu.org/pub/gnu/auctex/auctex-${VERSION}.tar.gz"
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  sh configure --prefix=$PREFIX \
    --with-lispdir=$PREFIX/share/emacs/site-lisp \
    --with-texmf-dir=$HOME/Library/texmf \
    > /dev/null 2>&1 || die "configure failed"
  make > /dev/null 2>&1 || die "make failed"
  make install > /dev/null 2>&1 || die "make install failed"
) || die "failed to build"

clean_files $dir
