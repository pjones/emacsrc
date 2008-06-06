#!/bin/sh

. `dirname $0`/common.sh
URL="git://github.com/tomtt/emacs-rails.git"
dir="emacs-rails"

test -d $dir || git clone $URL || die "failed to clone git repo"
test -d $dir || die "git repo didn't create $dir"

DEST=${PREFIX}/share/emacs/site-lisp/rails
mkdir -p $DEST
cp -v $dir/*.el $DEST/
rm -rf $dir
