{ pkgs, ... }:

{
  # Packages needed on any system, including servers, workstations,
  # laptops, VMs, etc.
  packages = with pkgs; [
    # Emacs, my best friend.  But it's better without GTK. (The Emacs
    # daemon works better without GTK, that is.)
    (emacs.override {withX = true; withGTK = false;})

    # I still have a lot of scripts that require Ruby.
    ruby_2_1

    tmux duplicity gnupg inotifyTools libxml2 libxslt pwgen
    rsync unison zip unzip
  ];
}
