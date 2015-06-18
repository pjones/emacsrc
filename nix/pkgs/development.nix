{ pkgs, ... }:

{
  # Packages for software development.
  packages = with pkgs; [
    # Core Tools like Text Editors:
    emacs

    # Revision Control Systems:
    gitAndTools.gitFull gitAndTools.gitAnnex darcs mr

    # Build tools:
    gnumake

    # Hardware Hacking.
    arduino_core picocom

    # JavaScript Development
    nodejs nodePackages.jshint
  ];
}
