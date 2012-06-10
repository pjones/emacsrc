#!/bin/sh

. `dirname $0`/common.sh
URL="git://repo.or.cz/org-mode.git"
SITE_LISP=$PREFIX/share/emacs/site-lisp
dir=`fetch_url $URL`

(
  cd $dir || die "WTF"
  git checkout $VERSION || die "bad tag: $VERSION"
  make clean > /dev/null 2>&1
  rm $SITE_LISP/org.el* $SITE_LISP/ob.el* $SITE_LISP/org-*.el* $SITE_LISP/ob-*.el*
  rm -rf $SITE_LISP/org
  mkdir -p $SITE_LISP/org
  make prefix=${PREFIX}/share install > /dev/null 2>&1 || die "make failed"
)

clean_files $dir
