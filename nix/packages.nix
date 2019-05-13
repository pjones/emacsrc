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
    # Newer versions of existing packages:
    passmm = import ./passmm.nix   { inherit super self pkgs melpaBuild; };
  });

in

################################################################################
# Emacs package list:
overrides.emacsWithPackages (epkgs: with epkgs; [
  electric-spacing              # Insert operators with surrounding spaces smartly
  adaptive-wrap                 # Smart line-wrapping with wrap-prefix
  async                         # Asynchronous processing in Emacs
  beginend                      # Redefine M-< and M-> for some modes
  captain                       # CAPiTalization is Automatic IN emacs
  company                       # Modular text completion framework
  company-ghc                   # company-mode ghc-mod backend
  company-ghci                  # company backend which uses the current ghci process
  company-quickhelp             # Popup documentation for completion candidates
  company-statistics            # Sort candidates using completion history
  counsel                       # Various completion functions using Ivy
  counsel-world-clock           # Display world clock using Ivy
  csv-mode                      # Major mode for editing comma/char separated values
  dante                         # Development mode for Haskell
  darkroom                      # Remove visual distractions and focus on writing
  default-text-scale            # Easily adjust the font size in all frames
  deft                          # quickly browse, filter, and edit plain text notes
  dictionary                    # Client for rfc2229 dictionary servers
  diff-hl                       # Highlight uncommitted changes using VC
  dired-filter                  # Ibuffer-like filtering for dired
  dired-narrow                  # Live-narrowing of search results for dired
  direnv                        # direnv support
  doom-modeline                 # A minimal and modern mode-line
  doom-themes                   # an opinionated pack of modern color-themes
  edit-server                   # server that responds to edit requests from Chrome
  eimp                          # Emacs Image Manipulation Package
  erc-hl-nicks                  # ERC nick highlighter that ignores uniquifying chars when colorizing
  evil                          # Extensible Vi layer for Emacs
  evil-collection               # A set of keybindings for Evil mode
  evil-commentary               # Comment stuff out. A port of vim-commentary
  evil-fringe-mark              # Display evil-mode marks in the fringe
  evil-leader                   # let there be <leader>
  evil-magit                    # evil-based key bindings for magit
  evil-nl-break-undo            # Break evil's undo sequence on CR
  evil-org                      # evil keybindings for org-mode
  evil-surround                 # emulate surround.vim from Vim
  evil-textobj-syntax           # Provides syntax text objects
  flycheck                      # On-the-fly syntax checking
  git-annex                     # Mode for easy editing of git-annex'd files
  graphviz-dot-mode             # Mode for the dot-language used by graphviz
  haskell-mode                  # A Haskell editing mode
  htmlize                       # Convert buffer text and decorations to HTML
  http                          # Yet another HTTP client
  hydra                         # Make bindings that stick around
  ialign                        # visual align-regexp
  indium                        # JavaScript Awesome Development Environment
  inf-ruby                      # Run a Ruby process in a buffer
  ivy                           # Incremental Vertical completion
  ivy-hydra                     # Additional key bindings for Ivy
  ivy-rich                      # More friendly display transformer for ivy
  js2-mode                      # Improved JavaScript editing mode
  json-mode                     # Major mode for editing JSON files
  magit                         # A Git porcelain inside Emacs
  magit-annex                   # Control git-annex from Magit
  markdown-mode                 # Major mode for Markdown-formatted text
  minions                       # A minor-mode menu for the mode line
  mu4e-query-fragments          # mu4e query fragments extension
  nix-mode                      # Major mode for editing .nix files
  no-littering                  # help keeping ~/.emacs.d clean
  noccur                        # Run multi-occur on project/dired files
  org                           # Outline-based notes management and organizer
  org-bullets                   # Show bullets in org-mode as UTF-8 characters
  org-clock-csv                 # Export `org-mode' clock entries to CSV format
  org-mru-clock                 # clock in/out of tasks with completion and persistent history
  org-tree-slide                # A presentation tool for org-mode
  orgalist                      # Manage Org-like lists in non-Org buffers
  passmm                        # A minor mode for pass (Password Store).
  pdf-tools                     # Support library for PDF documents
  pkgs.mu                       # maildir indexer/searcher + emacs mail client
  poly-erb                      # Polymode for erb
  poly-markdown                 # Polymode for markdown-mode
  polymode                      # Extensible framework for multiple major modes
  projectile                    # Manage and navigate projects in Emacs easily
  rainbow-mode                  # Colorize color names in buffers
  resize-window                 # easily resize windows
  ruby-end                      # Automatic insertion of end blocks for Ruby
  scad-mode                     # A major mode for editing OpenSCAD code
  shackle                       # Enforce rules for popups
  smartrep                      # Support sequential operation which omitted prefix keys
  smex                          # M-x interface with Ido-style fuzzy matching
  swiper                        # Isearch with an overview. Oh, man!
  switch-window                 # A *visual* way to switch window
  treemacs                      # A tree style file explorer package
  treemacs-evil                 # Evil mode integration for treemacs
  treemacs-projectile           # Projectile integration for treemacs
  typescript-mode               # Major mode for editing typescript
  visual-fill                   # Auto-refill paragraphs without modifying the buffer
  vlf                           # View Large Files
  which-key                     # Display available keybindings in popup
  yaml-mode                     # Major mode for editing YAML files
])
