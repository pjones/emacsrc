################################################################################
# Access the vmrun tool.
vmware_vmrun () {
  "/Library/Application Support/VMware Fusion/vmrun" $@
}

################################################################################
# Returns the base directory where we mount things.
vmware_base_mount_point () {
  echo ~/Develop/hosts
}

################################################################################
# Mount a VMWare virtual machine using sshfs
vmware_mount () {
  name=$1

  server="sshfs.${name}.pmade.com"
  directory=`vmware_base_mount_point`"/$name"
  
  mkdir -p $directory
  sshfs "${server}:" $directory -oauto_cache,reconnect,volname=$name
}

################################################################################
# Mount a virtual machine and create a tmux session
vmware_session () {
  name=$1
  
  vmware_mount $name
  directory=`vmware_base_mount_point`"/$name"
  (cd $directory && tmux new-session -d -s $name)
}
