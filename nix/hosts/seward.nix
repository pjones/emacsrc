# Packages to install for my laptop Seward.
let pkgs = import <nixpkgs> {}; in

(import ../lib {inherit pkgs;}).process {

  # Other files to include:
  imports = [
    ../pkgs/desktop.nix
  ];

  # Host specific packages:
  packages = [
  ];
}
