################################################################################
# Mount a virtual machine via SSHFS and then create a tmux session
# inside it.
tmux_mount () {
  if [ $# -ne 1 ]; then
    echo "Usage: tmux_mount name"
    return 1
  fi

  name=$1
  mount_point=$HOME/develop/hosts/$1
  mount_options="-oauto_cache,reconnect,ControlMaster=no"
  server=${name}.pmade.com

  # If the host is a VM and it's not already running, start it.
  virsh_is_registered $name && (virsh_running $name || virsh_start $name)

  echo "==> Mounting $name on $mount_point"
  mkdir -p $mount_point

  if ssh $server test -d develop; then
    server_dir=develop
  elif ssh $server test -d Develop; then
    server_dir=Develop
  else
    server_dir=""
  fi

  sshfs "${server}:${server_dir}" $mount_point $mount_options || return 1
  cp ~/.emacs.d/server/server $mount_point/emacs.server

  echo "==> Starting tmux session $name"
  (cd $mount_point && tmux new-session -d -s $name)

  return $?
}

################################################################################
# Mount and attach to a new session
tmux_mount_attach () {
  tmux_mount $1 && tmux attach -t $1
}

################################################################################
# Kill a tmux session and un-mount the sshfs file system.
tmux_umount () {
  if [ $# -ne 1 ]; then
    echo "Usage: tmux_umount name"
    return 1
  fi

  name=$1
  mount_point=$HOME/develop/hosts/$1

  if tmux list-sessions | awk '{print $1}' | grep -q ^${name}:\$; then
    echo "==> Killing tmux session $name"
    tmux kill-session -t $name || return 1
    sleep 2 # Wait for all processes accessing the VM to die
  fi

  # Avoid my 'df' alias below by giving the full path
  if /bin/df -P | awk 'NR > 1 {print $6}' | grep -q hosts/$name\$; then
    echo "==> Un-mounting $mount_point"
    fusermount -u $mount_point || return 1
  fi
}

################################################################################
# Runs tmux_umount and then stops the VM
tmux_umount_stop () {
  if [ $# -ne 1 ]; then
    echo "Usage: tmux_umount_stop name"
    return 1
  fi

  name=$1
  tmux_umount $name || return 1

  if virsh_running $name; then
    echo "==> Stopping $name virtual machine"
    virsh_stop $name || return 1
  fi
}
