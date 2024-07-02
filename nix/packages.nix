{ lib
, emacs
, emacsPackagesFor
, inputs
}:
let
  # Function to update several attributes in an Emacs package:
  update = pkg: src: pkg.overrideAttrs (orig: rec {
    inherit src;

    name = "emacs-${orig.ename or orig.pname}-${version}";
    version = "99999999.0";

    # When updating a package, automatically remove the broken flag:
    meta = (orig.meta or { }) // { broken = false; };
  });

  # Package overrides:
  emacsWithOverrides = (emacsPackagesFor emacs).overrideScope' (self: super: {
    passmm = update super.passmm inputs.passmm;
    telega = update super.telega inputs.telega;
  });
in
# Emacs package list:
emacsWithOverrides.emacsWithPackages (epkgs: with epkgs; [
  ace-window # Quickly switch windows
  adaptive-wrap # Smart line-wrapping with wrap-prefix
  async # Asynchronous processing in Emacs
  avy # Jump to arbitrary positions in visible text and select text quickly
  cape # Let your completions fly!
  catppuccin-theme # Soothing pastel theme for Emacs
  clojure-mode # Major mode for Clojure code
  consult # Consulting completing-read
  consult-org-roam # Consult integration for org-roam
  corfu # Completion Overlay Region FUnction
  csv-mode # Major mode for editing comma/char separated values
  darkroom # Remove visual distractions and focus on writing
  diff-hl # Highlight uncommitted changes using VC
  dired-filter # Ibuffer-like filtering for dired
  dired-narrow # Live-narrowing of search results for dired
  dired-subtree # Insert subdirectories in a tree-like fashion
  dracula-theme # The most famous dark theme ever created.
  dumb-jump # Jump to definition for 40+ languages without configuration
  ef-themes # Colourful (“pretty”) yet legible themes
  eimp # Emacs Image Manipulation Package
  eldoc # Show function arglist or variable docstring in echo area
  elm-mode # Elm mode for emacs
  embark # Conveniently act on minibuffer completions
  embark-consult # Consult integration for Embark
  ement # A Matrix client for GNU Emacs.
  emms # The Emacs Multimedia System
  envrc # Support for `direnv' that operates buffer-locally
  erc-hl-nicks # ERC nick highlighter that ignores uniquifying chars when colorizing
  expand-region # Increase selected region by semantic units.
  flycheck # On-the-fly syntax checking
  flycheck-indicator # A fancy mode line indicator for `flycheck-mode'
  flycheck-rust # Rust additions and Cargo support
  git-annex # Mode for easy editing of git-annex'd files
  go-mode # Major mode for the Go programming language
  goto-chg # Go to last change
  graphviz-dot-mode # Mode for the dot-language used by graphviz
  haskell-mode # A Haskell editing mode
  highlight-indent-guides # Minor mode to highlight indentation
  hl-todo # Highlight TODO and similar keywords
  htmlize # Convert buffer text and decorations to HTML.
  http # Yet another HTTP client
  indium # JavaScript Awesome Development Environment
  inf-ruby # Run a Ruby process in a buffer
  jinx # Enchanted Spell Checker
  js2-mode # Improved JavaScript editing mode
  json-mode # Major mode for editing JSON files
  jsonrpc # JSON-RPC library
  kaolin-themes # A set of eye pleasing themes
  khardel # integrating khard, a console cardav client
  link-hint # Use avy to open, copy, etc. visible links
  magit # A Git porcelain inside Emacs
  magit-annex # Control git-annex from Magit
  marginalia # Enrich existing commands with completion annotations
  markdown-mode # Major mode for Markdown-formatted text
  mermaid-mode # Emacs major mode for working with mermaid graphs
  minions # A minor-mode menu for the mode line
  nix-mode # Major mode for editing .nix files
  no-littering # help keeping ~/.emacs.d clean
  noccur # Run multi-occur on project/dired files
  nov # Featureful EPUB reader mode
  ob-mermaid # Generate mermaid diagrams within Emacs org-mode babel
  orderless # Completion style for matching regexps in any order
  org # Outline-based notes management and organizer
  org-appear # Make invisible parts of Org elements appear visible.
  org-bulletproof # Automatic bullet cycling for Org mode
  org-clock-csv # Export `org-mode' clock entries to CSV format
  org-mime # Send HTML email using Org-mode HTML export
  org-roam # A database abstraction layer for Org-mode
  org-tree-slide # A presentation tool for org-mode
  orgalist # Manage Org-like lists in non-Org buffers
  ox-gfm # Github Flavored Markdown Back-End for Org Export Engine
  package-lint # A linting library for elisp package authors
  passmm # A minor mode for pass (Password Store).
  password-store # Password store (pass) support
  pdf-tools # Support library for PDF documents
  plantuml-mode # A major mode for editing PlantUML sources in Emacs
  poet-theme # An emacs theme that's well suited for modes using variable pitch
  prescient # Better sorting and filtering
  project # Operations on the current project
  puni # Parentheses Universalistic
  purescript-mode # A PureScript editing mode
  rainbow-mode # Colorize color names in buffers
  reformatter # Define commands which run reformatters on the current Emacs buffer
  resize-window # easily resize windows
  rg # A search tool based on ripgrep
  rotate # Rotate the layout of emacs
  ruby-end # Automatic insertion of end blocks for Ruby
  rust-mode # A major-mode for editing Rust source code
  sage-shell-mode # A front-end for Sage Math
  scad-mode # A major mode for editing OpenSCAD code
  telega # Telegram client (unofficial)
  treesit-grammars.with-all-grammars # For use with tree-sitter
  tron-legacy-theme # Original retro-futuristic theme inspired by Tron: Legacy
  typescript-mode # Major mode for editing typescript
  vertico # VERTical Interactive COmpletion
  visual-fill # Auto-refill paragraphs without modifying the buffer
  vlf # View Large Files
  vterm # Fully-featured terminal emulator
  weyland-yutani-theme # Emacs theme based off Alien movie franchise
  wgrep # Writable grep buffer and apply the changes to files
  which-key # Display available keybindings in popup
  winum # Navigate windows and frames using numbers.
  yaml-mode # Major mode for editing YAML files
  yasnippet # Yet another snippet extension for Emacs
])
