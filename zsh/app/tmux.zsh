################################################################################
# Mount a virtual machine via SSHFS and then create a tmux session
# inside it.
tmux_mount () {
  if [ $# -ne 1 ]; then
    echo "Usage: tmux_mount name"
    return 1
  fi

  name=$1
  mount_point=$HOME/mnt/$1
  server=sshfs.${name}.pmade.com
  
  mkdir -p $mount_point
  sshfs "${server}:develop" $mount_point -oauto_cache,reconnect
  (cd $mount_point && tmux new-session -d -s $name)
  
  return $?
}

################################################################################
# Mount and attach to a new session
tmux_mount_attach () {
  tmux_mount $1 && tmux attach -t $1
}
