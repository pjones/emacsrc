# Packages to install for my laptop Seward.
let pkgs = import <nixpkgs> {}; in
import ../pkgs/desktop.nix { inherit pkgs; }
