#!/bin/sh

. `dirname $0`/common.sh
URL="git://github.com/tomtt/emacs-rails.git"
DEST=${PREFIX}/share/emacs/site-lisp/rails
dir="emacs-rails"

if [ $VERSION = 'delete' ]; then
  rm -rf $DEST
  exit
fi

test -d $dir || git clone $URL || die "failed to clone git repo"
test -d $dir || die "git repo didn't create $dir"

mkdir -p $DEST
cp -v $dir/*.el $DEST/
rm -rf $dir
