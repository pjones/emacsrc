# Packages for software development.
{ pkgs, ... }:

with pkgs; {
  # Core Tools like Text Editors:
  inherit emacs vim;

  # Revision Control Systems:
  git = gitAndTools.gitFull;
  inherit darcs;

  # Build tools:
  inherit gnumake;
}
