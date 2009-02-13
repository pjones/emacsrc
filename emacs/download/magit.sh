. `dirname $0`/common.sh
URL="git://gitorious.org/magit/mainline.git"
dir=`fetch_url $URL magit.git`

(
  cd $dir || die "WTF"
  DEST=${PREFIX}/share/emacs/site-lisp
  rm -rf $DEST/magit*
  cp -p magit.el $DEST/
)

clean_files $dir
