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

  # Start the VM if it's not already running
  virsh_running $name || virsh_start $name

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

################################################################################
# Returns 0 if the given tmux session already exists.
tmux_has_session () {
  if [ $# -ne 1 ]; then
    echo "Usage: tmux_has_session name"
    return 2
  fi

  name=$1

  tmux has-session -t $name > /dev/null 2>&1
  return $?
}

################################################################################
# Start a new tmux session.  This function takes three parameters and
# all of them must be the names of variables that hold the requested
# information, and not the values themselves.
#
# $1: The name of the session
# $2: A list of windows to create (array)
# $3: The directories for those windows (array)
tmux_new_session () {
  if [ $# -ne 3 ]; then
    echo "Usage: tmux_new_session name windows dirs"
    return 1
  fi

  if [ ! -z "$TMUX" ]; then
    echo "Please detach from the current tmux first!"
    return 1
  fi

  typeset -a w
  typeset -a d

  n=${(P)1}
  w=(${(P)2})
  d=(${(P)3})

  if ! tmux_has_session $n; then
    (cd ${d[1]} && tmux new-session -d -s $n -n ${w[1]})

    for (( i=2; i <= $#w; i++)); do
      (cd ${d[i]} && tmux new-window -d -n ${w[i]})
    done
  fi

  tmux attach-session -t $n
}

################################################################################
# Start my "home" tmux session
tmux_session_home () {
  name=home

  typeset -a windows
  windows=(rc privrc clients down)

  typeset -a dirs
  dirs=(~/develop/pmade/rc ~/develop/pmade/privaterc
    ~/documents/clients ~/download)

  tmux_new_session name windows dirs
}

################################################################################
# Start my "bootcamp" tmux session
tmux_session_bootcamp () {
  typeset -a windows
  typeset -a dirs

  if [ -d ~/documents/books/bootcamp ]; then
    bootcamp=~/documents/books/bootcamp
  elif [ -d ~/documents/writing/books/bootcamp ]; then
    bootcamp=~/documents/writing/books/bootcamp
  else
    echo "where is the bootcamp dir?"
    return 1
  fi

  name=bootcamp
  windows=(bootcamp code quiz)
  dirs=($bootcamp $bootcamp/coders $bootcamp/quiz)
  tmux_new_session name windows dirs
}

################################################################################
# Start my "classroom" tmux session
tmux_session_classroom () {
  typeset -a windows
  typeset -a dirs

  name=classroom
  windows=(bootcamp)
  dirs=~/documents/books/bootcamp
  tmux_new_session name windows dirs
}
