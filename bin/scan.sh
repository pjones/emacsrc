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
scanimage --mode $mode --resolution $resolution $size > ${base}.pnm
convert ${base}.pnm ${base}.pdf
rm ${base}.pnm

################################################################################
echo "created ${base}.pdf"
