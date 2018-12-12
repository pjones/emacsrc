{ pkgs
, emacs
, ...
}:

let
  ##############################################################################
  # Access to the MELPA Emacs builder:
  melpaBuild = import <nixpkgs/pkgs/build-support/emacs/melpa.nix> {
    inherit (pkgs) lib stdenv texinfo fetchFromGitHub;
    inherit emacs;
  };

  ##############################################################################
  # Package overrides:
  overrides = (pkgs.emacsPackagesNgGen emacs).overrideScope' (self: super: {
    # Use unstable versions:
    default-text-scale = self.melpaPackages.default-text-scale;
    projectile = self.melpaPackages.projectile;

    # Newer versions of existing packages:
    exwm = import ./exwm.nix { inherit super self pkgs melpaBuild; };
    xelb = import ./xelb.nix { inherit super self pkgs melpaBuild; };
    mu4e-query-fragments = import ./mu4e-query-fragments.nix { inherit super self pkgs melpaBuild; };

    # My custom packages:
    exwm-nw  = import ./exwm-nw.nix  { inherit super self pkgs melpaBuild; };
    ivy-exwm = import ./ivy-exwm.nix { inherit super self pkgs melpaBuild; };
    passmm   = import ./passmm.nix   { inherit super self pkgs melpaBuild; };
  });

in

################################################################################
# Emacs package list:
overrides.emacsWithPackages (epkgs: with epkgs; [
  adaptive-wrap
  alect-themes
  async
  beginend
  company
  company-ghc
  company-quickhelp
  company-statistics
  counsel
  counsel-projectile
  counsel-world-clock
  dante
  default-text-scale
  deft
  dired-filter
  dired-narrow
  dired-sidebar
  dired-subtree
  edit-server
  eimp
  evil
  evil-collection
  evil-commentary
  evil-fringe-mark
  evil-leader
  evil-magit
  evil-org
  evil-quickscope
  evil-surround
  exwm
  exwm-nw
  flycheck
  git-annex
  graphviz-dot-mode
  haskell-mode
  highlight-indent-guides
  htmlize
  http
  hydra
  ialign
  indium
  inf-ruby
  ivy
  ivy-exwm
  ivy-hydra
  ivy-rich
  js2-mode
  magit
  magit-annex
  markdown-mode
  mu4e-query-fragments
  nix-mode
  no-littering
  noccur
  org
  org-tree-slide
  passmm
  pdf-tools
  pkgs.mu
  projectile
  rainbow-mode
  resize-window
  ruby-end
  scad-mode
  shackle
  spaceline
  swiper
  switch-window
  typescript-mode
  which-key
  yaml-mode
])
