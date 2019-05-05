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
  overrides = (pkgs.emacsPackagesNgGen emacs).overrideScope' (self: super: rec {
    # Use unstable versions:
    default-text-scale = self.melpaPackages.default-text-scale;
    evil-org = self.melpaPackages.evil-org;
    projectile = self.melpaPackages.projectile;

    # Newer versions of existing packages:
    passmm = import ./passmm.nix   { inherit super self pkgs melpaBuild; };
    doom-modeline = import ./doom-modeline.nix { inherit self pkgs melpaBuild shrink-path; };
    doom-themes = import ./doom-themes.nix { inherit self pkgs melpaBuild; };
    polymode = import ./polymode.nix { inherit pkgs melpaBuild; };
    poly-markdown = import ./poly-markdown.nix { inherit pkgs melpaBuild self polymode; };
    poly-erb = import ./poly-erb.nix { inherit pkgs melpaBuild polymode; };

    # GitLab bugs: https://github.com/NixOS/nixpkgs/issues/48215
    mu4e-query-fragments = import ./mu4e-query-fragments.nix { inherit super self pkgs melpaBuild; };
    shrink-path = import ./shrink-path.nix { inherit super self pkgs melpaBuild; };
  });

in

################################################################################
# Emacs package list:
overrides.emacsWithPackages (epkgs: with epkgs; [
  adaptive-wrap
  async
  beginend
  company
  company-ghc
  company-quickhelp
  company-statistics
  counsel
  counsel-world-clock
  dante
  default-text-scale
  deft
  dictionary
  dired-filter
  dired-narrow
  direnv
  doom-modeline
  doom-themes
  edit-server
  eimp
  erc-hl-nicks
  evil
  evil-collection
  evil-commentary
  evil-fringe-mark
  evil-leader
  evil-magit
  evil-org
  evil-surround
  flycheck
  git-annex
  graphviz-dot-mode
  haskell-mode
  htmlize
  http
  hydra
  ialign
  indium
  inf-ruby
  ivy
  ivy-hydra
  ivy-rich
  js2-mode
  magit
  magit-annex
  markdown-mode
  minions
  mu4e-query-fragments
  nix-mode
  no-littering
  noccur
  org
  org-bullets
  org-clock-csv
  org-mru-clock
  org-tree-slide
  passmm
  pdf-tools
  pkgs.mu
  projectile
  polymode
  poly-erb
  poly-markdown
  rainbow-mode
  resize-window
  ruby-end
  scad-mode
  shackle
  smex
  swiper
  switch-window
  treemacs
  treemacs-evil
  treemacs-projectile
  typescript-mode
  which-key
  yaml-mode
])
