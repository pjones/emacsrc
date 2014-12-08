#!/bin/sh

################################################################################
# Uses xkbcomp to change Xorg keybindings.  This is suppose to be
# better than xmodmap but I think it's worse.
XKB_PATH="-I$HOME/.xkb"

# On NixOS we need to point back to the original xkb share directory.
if [ -d /nix/store ]; then
    exe=`which setxkbmap`
    real=`realpath $exe`
    bin=`dirname $real`
    base=`dirname $bin`
    XKB_PATH="$XKB_PATH -I$base/share/X11/xkb"
fi

################################################################################
# Make a new keymap if one doesn't exist.
XKB_KEYMAP=$HOME/.xkb/keymap

if [ ! -r $XKB_KEYMAP ]; then
    setxkbmap -print |\
      sed 's/symbols\(.*\)"/symbols\1+custom(capsrctl)+custom(lctlmod3)"/' \
      > $XKB_KEYMAP
fi

################################################################################
# Now activate the keys.
xkbcomp ${=XKB_PATH} $XKB_KEYMAP $DISPLAY > /dev/null
