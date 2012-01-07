################################################################################
wmctrl_activate_emacs () {
  win_id=$(wmctrl -l |awk '$4 ~ /emacs/ && $2 == 0 {print $1}'|head -1)
  
  if [[ -n $win_id ]]; then
    wmctrl -ia $win_id
  fi
}

################################################################################
# Activates the biggest Conkeror window.
wmctrl_activate_conkeror () {
  wmctrl -ia $(wmctrl -lG|grep Conkeror|sort -nk 5|tail -1|awk '{print $1}')
}
