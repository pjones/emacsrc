#!/bin/sh

# Load in common items
. `dirname $0`/common.sh

URL="http://ess.r-project.org/downloads/ess/ess-${VERSION}.tgz"
dir=`fetch_url $URL`

dest=$PREFIX/share/emacs/site-lisp/ess
rm -rf $dest
cp -r $dir $dest
clean_files $dir
