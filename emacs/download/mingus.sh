#!/bin/sh

# Mingus: a mpd client for Emacs
# https://github.com/pft/mingus

. `dirname $0`/common.sh
URL="https://github.com/pft/mingus.git"
dir=`fetch_url $URL`
dest=${PREFIX}/share/emacs/site-lisp/mingus

(
  cd $dir || die "WTF"
  git checkout $VERSION || die "bad tag: $VERSION"
  mkdir -p $dest
  cp *.el $dest/
)

clean_files $dir
