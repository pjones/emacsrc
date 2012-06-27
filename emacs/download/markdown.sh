#!/bin/sh

. `dirname $0`/common.sh
URL="git://pmade.com/markdown-mode.git"
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  cp markdown-mode.el $SITE_LISP/
)

clean_files $dir
