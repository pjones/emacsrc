# Packages needed on any system, including servers, workstations,
# laptops, VMs, etc.
{ pkgs, ... }:

with pkgs; {
  inherit emacs tmux;
}
