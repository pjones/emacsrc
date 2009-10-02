#!/bin/sh

# Load in common items
. `dirname $0`/common.sh

URL="http://ftp.gnu.org/pub/gnu/auctex/auctex-${VERSION}.tar.gz"
dir=`fetch_url $URL`

TEXMF=$HOME/Library/texmf
mkdir -p $TEXMF

(
  cd $dir || die "WTF"
  sh configure --prefix=$PREFIX \
    --with-lispdir=$PREFIX/share/emacs/site-lisp \
    --with-texmf-dir=$TEXMF \
    || die "configure failed"
  make || die "make failed"
  make install || die "make install failed"
) || die "failed to build"

clean_files $dir
