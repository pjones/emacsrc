#!/bin/sh

. `dirname $0`/common.sh
URL="git://jblevins.org/git/markdown-mode.git"
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  cp markdown-mode.el $SITE_LISP/
)

clean_files $dir
