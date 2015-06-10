################################################################################
function x11vnc-projector () {
  projector=`xrandr|egrep '^HDMI1[[:space:]]+connected'|cut -f3 -d' '`

  x11vnc -clip "$projector" -viewonly \
         -forever -localhost -quiet \
         -wirecopyrect -scrollcopyrect
}
