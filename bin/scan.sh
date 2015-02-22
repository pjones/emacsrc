#!/bin/sh

################################################################################
set -e

################################################################################
if [ -d ~/.nix-profile/etc/sane.d/ ]; then
  export LD_LIBRARY_PATH=~/.nix-profile/lib/sane
  export SANE_CONFIG_DIR=~/.nix-profile/etc/sane.d/
fi

################################################################################
mode=Gray # Color
resolution=150
size="-x 215.9 -y 279.4" # Letter
base=`date +%Y-%m-%d_%H:%M:%S`
multipe=NO
current_page=1

################################################################################
while getopts "mb:" o; do
  case "${o}" in
    m) multipe=YES
       ;;

    b) base=$OPTARG
       ;;

    *) echo "Bad arguments"
       exit 1
       ;;
  esac
done

shift $((OPTIND-1))

################################################################################
if [ $# -gt 0 ]; then
  base=$1
  shift
fi

################################################################################
if [ -r ${base}.pnm -o -r ${base}.pdf ]; then
  echo "ERROR: $base basename is already taken!"
  exit 1
fi

################################################################################
while [ $multipe = YES -o $current_page -eq 1 ]; do
  if [ $multipe = YES ]; then
    file_name=`printf %s-%2d.pnm $base $current_page`
  else
    file_name=${base}.pnm
  fi

  scanimage --mode $mode --resolution $resolution $size > $file_name
  current_page=`expr 1 + $current_page`

  if [ $multipe = YES ]; then
    printf %s "Next page or quit (RET or q): "
    read answer
    [ "$answer" = "q" -o "$answer" = "Q" ] && break
  fi
done

if [ $multipe = YES ]; then
  convert ${base}-*.pnm ${base}.pdf
  rm ${base}-*.pnm
else
  convert ${base}.pnm ${base}.pdf
  rm ${base}.pnm
fi

################################################################################
echo "created ${base}.pdf"
