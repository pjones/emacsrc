################################################################################
wmctrl_activate_emacs () {
  win_id=$(wmctrl -l |awk '$4 ~ /emacs/ && $2 == 0 {print $1}'|head -1)
  
  if [[ -n $win_id ]]; then
    wmctrl -ia $win_id
  fi
}
