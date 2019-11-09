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
  overrides = (pkgs.emacsPackagesFor emacs).overrideScope' (self: super: rec {
    # Newer versions of existing packages:
    passmm = import ./passmm.nix   { inherit super self pkgs melpaBuild; };
  });

in

################################################################################
# Emacs package list:
overrides.emacsWithPackages (epkgs: with epkgs; [
  adaptive-wrap                 # Smart line-wrapping with wrap-prefix
  async                         # Asynchronous processing in Emacs
  beginend                      # Redefine M-< and M-> for some modes
  company                       # Modular text completion framework
  company-posframe              # Use a posframe as company candidate menu
  company-quickhelp             # Popup documentation for completion candidates
  company-statistics            # Sort candidates using completion history
  counsel                       # Various completion functions using Ivy
  counsel-world-clock           # Display world clock using Ivy
  csv-mode                      # Major mode for editing comma/char separated values
  cyberpunk-theme               # Cyberpunk Color Theme
  dante                         # Development mode for Haskell
  darkroom                      # Remove visual distractions and focus on writing
  deadgrep                      # fast, friendly searching with ripgrep
  default-text-scale            # Easily adjust the font size in all frames
  deft                          # quickly browse, filter, and edit plain text notes
  dictionary                    # Client for rfc2229 dictionary servers
  diff-hl                       # Highlight uncommitted changes using VC
  dired-filter                  # Ibuffer-like filtering for dired
  dired-narrow                  # Live-narrowing of search results for dired
  dired-subtree                 # Insert subdirectories in a tree-like fashion
  direnv                        # direnv support
  doom-modeline                 # A minimal and modern mode-line
  dracula-theme                 # Dracula Theme
  edit-server                   # server that responds to edit requests from Chrome
  eimp                          # Emacs Image Manipulation Package
  elm-mode                      # Elm mode for emacs
  erc-hl-nicks                  # ERC nick highlighter that ignores uniquifying chars when colorizing
  evil                          # Extensible Vi layer for Emacs
  evil-collection               # A set of keybindings for Evil mode
  evil-commentary               # Comment stuff out. A port of vim-commentary
  evil-fringe-mark              # Display evil-mode marks in the fringe
  evil-leader                   # let there be <leader>
  evil-magit                    # evil-based key bindings for magit
  evil-matchit                  # Vim matchit ported to Evil
  evil-nl-break-undo            # Break evil's undo sequence on CR
  evil-org                      # evil keybindings for org-mode
  evil-owl                      # Preview evil registers and marks before using them
  evil-surround                 # emulate surround.vim from Vim
  evil-textobj-syntax           # Provides syntax text objects
  flycheck                      # On-the-fly syntax checking
  flycheck-posframe             # Show flycheck error messages using posframe.el
  forge                         # Access Git forges from Magit
  git-annex                     # Mode for easy editing of git-annex'd files
  graphviz-dot-mode             # Mode for the dot-language used by graphviz
  haskell-mode                  # A Haskell editing mode
  hasky-extensions              # Toggle Haskell language extensions
  htmlize                       # Convert buffer text and decorations to HTML
  http                          # Yet another HTTP client
  ialign                        # visual align-regexp
  indium                        # JavaScript Awesome Development Environment
  inf-ruby                      # Run a Ruby process in a buffer
  ivy                           # Incremental Vertical completion
  ivy-posframe                  # Using posframe to show Ivy
  ivy-rich                      # More friendly display transformer for ivy
  js2-mode                      # Improved JavaScript editing mode
  json-mode                     # Major mode for editing JSON files
  kaolin-themes                 # A set of eye pleasing themes
  magit                         # A Git porcelain inside Emacs
  magit-annex                   # Control git-annex from Magit
  markdown-mode                 # Major mode for Markdown-formatted text
  minions                       # A minor-mode menu for the mode line
  monokai-theme                 # A fruity color theme for Emacs
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
  wgrep                         # Writable grep buffer and apply the changes to files
  which-key                     # Display available keybindings in popup
  which-key-posframe            # Using posframe to show which-key
  yaml-mode                     # Major mode for editing YAML files
])
