{ pkgs, ... }:

{
  # Packages needed on any system, including servers, workstations,
  # laptops, VMs, etc.
  packages = with pkgs; [
    emacs tmux duplicity gnupg inotifyTools libxml2 libxslt pwgen
    rsync unison zip unzip
  ];
}
