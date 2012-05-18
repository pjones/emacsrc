################################################################################
export VIRSH_DEFAULT_CONNECT_URI="qemu:///system"

################################################################################
# Returns 0 if a VM is running, otherwise 1.
virsh_running () {
  if [ $# -ne 1 ]; then
    echo "Usage: virsh_running name"
    return 2
  fi

  name=$1
  state=`virsh dominfo $name | grep ^State | awk '{print $2}'`

  if [ "$state" = "running" ]; then
    return 0
  else
    return 1
  fi
}

################################################################################
# Returns 0 if the VM has managed save info, otherwise 1.
virsh_has_managed_save () {
  if [ $# -ne 1 ]; then
    echo "Usage: virsh_has_managed_save name"
    return 2
  fi

  name=$1
  save=`virsh dominfo $name | grep '^Managed save' | awk '{print $3}'`

  if [ "$save" = "yes" ]; then
    return 0
  else
    return 1
  fi
}

################################################################################
# Start a VM and wait for it to be running.
virsh_start () {
  if [ $# -ne 1 ]; then
    echo "Usage: virsh_start name"
    return 1
  fi

  name=$1
  hostname="${name}.pmade.com"

  # First check to see if we're already running.
  virsh_running $name && return 0

  # Record if we have managed state
  if virsh_has_managed_save $name; then
    with_save=1
  else
    with_save=0
  fi

  # Start it and wait for it to be pingable
  virsh start $name || return 1
  echo "==> Waiting for $name to start..."
  [ $with_save -eq 0 ] && sleep 30
  ping -c 1 -q -w 120 $hostname
}

################################################################################
# Stop a running VM using managedsave.
virsh_stop () {
  if [ $# -ne 1 ]; then
    echo "Usage: virsh_stop name"
    return 1
  fi

  name=$1

  # Can't stop a domain unless it's running
  virsh_running $name || return 0

  echo "==> Waiting for $name to stop"
  virsh managedsave $name --verbose
}
