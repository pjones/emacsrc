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

    # Haskell:
    haskellPackages.ghc
    haskellPackages.cabalInstall
    haskellPackages.alex
    haskellPackages.happy

    # Hardware Hacking.
    arduino_core picocom
  ];
}
