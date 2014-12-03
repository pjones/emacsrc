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

  # Haskell:
  ghc = haskellPackages.ghc;
  cabal = haskellPackages.cabalInstall;
  alex = haskellPackages.alex;
  happy = haskellPackages.happy;
}
