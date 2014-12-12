{ pkgs, ... }:

{
  # Packages for software development.
  packages = with pkgs; [
    # Core Tools like Text Editors:
    emacs

    # Revision Control Systems:
    gitAndTools.gitFull darcs

    # Build tools:
    gnumake

    # Hardware Hacking.
    # arduino_core picocom FIXME: remove these at some point.
  ];
}
