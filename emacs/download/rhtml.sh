#!/bin/sh

. `dirname $0`/common.sh
URL="http://rinari.rubyforge.org/svn/trunk/rhtml"
svn co $URL || die "failed to svn co rhtml"
mv rhtml $PREFIX/share/emacs/site-lisp/ || die "failed to move rhtml to site-lisp"
