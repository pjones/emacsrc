#!/bin/sh

export LD_LIBRARY_PATH=~/.nix-profile/lib/sane
export SANE_CONFIG_DIR=~/.nix-profile/etc/sane.d/

mode=Color
resolution=150
size="-x 215.9 -y 279.4" # Letter

scanimage --mode $mode --resolution $resolution $size > scan.pnm
