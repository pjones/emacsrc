{ emacs
, emacsPackagesFor
, inputs
}:
let
  # Version number to use for custom packages:
  version = "99999999.0";

  # Function to update several attributes in an Emacs package:
  update = pkg: src: pkg.overrideAttrs (orig: rec {
    inherit version src;
    name = "emacs-${orig.ename or orig.pname}-${version}";

    # When updating a package, automatically remove the broken flag:
    meta = (orig.meta or { }) // { broken = false; };
  });

  # Package overrides:
  emacsWithOverrides = (emacsPackagesFor emacs).overrideScope (self: super: {
    anki-editor = update super.anki-editor inputs.anki-editor;
    meow = update super.meow inputs.meow-edit;
    org-roam = update super.org-roam inputs.org-roam;

    # Not in nixpkgs:
    corg = emacs.pkgs.trivialBuild {
      inherit version;
      pname = "corg";
      src = inputs.corg;
      packageRequires = [ self.org super.s super.dash ];
    };

    nextflow-mode = emacs.pkgs.elpaBuild {
      inherit version;
      pname = "nextflow-mode";
      src = "${inputs.nextflow-mode}/nextflow-mode.el";
      packageRequires = [ super.groovy-mode ];
    };

    org-capture-ref = emacs.pkgs.trivialBuild {
      inherit version;
      pname = "org-capture-ref";
      src = inputs.org-capture-ref;
      packageRequires = [ super.compat self.persid ];
    };

    org-clock-dbus = emacs.pkgs.elpaBuild {
      inherit version;
      pname = "org-clock-dbus";
      src = "${inputs.org-clock-dbus}/lisp/org-clock-dbus.el";
    };

    org-grader = emacs.pkgs.elpaBuild {
      inherit version;
      pname = "org-grader";
      src = "${inputs.org-grader}/org-grader.el";
      packageRequires = [ self.org ];
    };

    ox-ipynb = emacs.pkgs.trivialBuild {
      inherit version;
      name = "ox-ipynb";
      src = inputs.ox-ipynb;
      packageRequires = [ super.s super.dash ];
    };

    persid = emacs.pkgs.trivialBuild {
      inherit version;
      pname = "persid";
      src = inputs.persid;
    };
  });
in
# Emacs package list:
emacsWithOverrides.emacsWithPackages (epkgs: with epkgs; [
  ace-window # Quickly switch windows
  anki-editor # Emacs minor mode for making Anki cards with Org Mode
  async # Asynchronous processing in Emacs
  avy # Jump to arbitrary positions in visible text and select text quickly
  cape # Let your completions fly!
  catppuccin-theme # Soothing pastel theme for Emacs
  clojure-mode # Major mode for Clojure code
  consult # Consulting completing-read
  consult-org-roam # Consult integration for org-roam
  consult-recoll # recoll queries in emacs using consult
  corfu # Completion Overlay Region FUnction
  corfu-prescient # Prescient support for corfu
  corg # Auto complete org-mode headers seamlessly
  csv-mode # Major mode for editing comma/char separated values
  darkroom # Remove visual distractions and focus on writing
  devdocs # Emacs viewer for DevDocs
  diff-hl # Highlight uncommitted changes using VC
  dired-filter # Ibuffer-like filtering for dired
  dired-narrow # Live-narrowing of search results for dired
  dired-subtree # Insert subdirectories in a tree-like fashion
  dockerfile-mode # An emacs mode for handling Dockerfiles
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
  ess # Emacs Speaks Statistics
  git-annex # Mode for easy editing of git-annex'd files
  go-mode # Major mode for the Go programming language
  google-translate # Emacs interface to Google Translate
  goto-chg # Go to last change
  graphviz-dot-mode # Mode for the dot-language used by graphviz
  haskell-mode # A Haskell editing mode
  hl-todo # Highlight TODO and similar keywords
  htmlize # Convert buffer text and decorations to HTML.
  http # Yet another HTTP client
  indent-bars # Fast, configurable indentation guide-bars for Emacs
  indium # JavaScript Awesome Development Environment
  inf-ruby # Run a Ruby process in a buffer
  jinx # Enchanted Spell Checker
  jq-mode # Emacs major mode for editing jq queries
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
  mastodon # Emacs client for fediverse servers that implement the Mastodon API.
  meow # Yet another modal editing on Emacs
  meow-tree-sitter # Tree-sitter powered 🌳 motions for Meow 🐱
  mermaid-mode # Emacs major mode for working with mermaid graphs
  minions # A minor-mode menu for the mode line
  modus-themes # Highly accessible themes for GNU Emacs
  mu4e # Mu4e, the mu mail user agent
  nextflow-mode # Emacs major mode for Nextflow
  nix-mode # Major mode for editing .nix files
  nix-ts-mode # An Emacs major mode for editing Nix expressions
  no-littering # help keeping ~/.emacs.d clean
  noccur # Run multi-occur on project/dired files
  nov # Featureful EPUB reader mode
  ob-mermaid # Generate mermaid diagrams within Emacs org-mode babel
  org # Outline-based notes management and organizer
  org-appear # Make invisible parts of Org elements appear visible.
  org-bulletproof # Automatic bullet cycling for Org mode
  org-capture-ref # Extract metadata/bibtex info from websites for org-capture
  org-clock-csv # Export `org-mode' clock entries to CSV format
  org-clock-dbus # Monitor org-clock from outside Emacs
  org-edna # Extensible Dependencies ’N’ Actions (EDNA) for Org Mode tasks
  org-grader # Support for grading papers in orgmode
  org-mime # Send HTML email using Org-mode HTML export
  org-modern # Modern Org Style.
  org-ref # citations, cross-references, bibliographies in org-mode
  org-roam # A database abstraction layer for Org-mode
  org-transclusion # Enable transclusion with Org Mode
  org-tree-slide # A presentation tool for org-mode
  orgalist # Manage Org-like lists in non-Org buffers
  ox-gfm # Github Flavored Markdown Back-End for Org Export Engine
  ox-ipynb # org-mode exporter to Jupyter notebooks
  package-lint # A linting library for elisp package authors
  pass # A major mode for password-store
  password-store # Password store (pass) support
  pdf-tools # Support library for PDF documents
  plantuml-mode # A major mode for editing PlantUML sources in Emacs
  poet-theme # An emacs theme that's well suited for modes using variable pitch
  prescient # Simple but effective sorting and filtering for Emacs
  project # Operations on the current project
  puni # Parentheses Universalistic
  purescript-mode # A PureScript editing mode
  rainbow-mode # Colorize color names in buffers
  rebecca-theme # The purple turtle theme for Spacemacs
  reformatter # Define commands which run reformatters on the current Emacs buffer
  resize-window # easily resize windows
  rg # A search tool based on ripgrep
  rg-themes # A collection of light and dark Emacs themes that are not hash on the eyes.
  rotate # Rotate the layout of emacs
  ruby-end # Automatic insertion of end blocks for Ruby
  rust-mode # A major-mode for editing Rust source code
  sage-shell-mode # A front-end for Sage Math
  scad-mode # A major mode for editing OpenSCAD code
  separedit # Edit comment or string/docstring or code
  timu-rouge-theme # Color theme inspired by the Rouge Theme for VSCode
  treesit-auto # Automatic installation, usage, and fallback for tree-sitter major modes
  treesit-grammars.with-all-grammars # For use with tree-sitter
  tron-legacy-theme # Original retro-futuristic theme inspired by Tron: Legacy
  typescript-mode # Major mode for editing typescript
  vertico # VERTical Interactive COmpletion
  vertico-prescient # Prescient support for vertico
  visual-fill # Auto-refill paragraphs without modifying the buffer
  vlf # View Large Files
  vterm # Fully-featured terminal emulator
  vue-mode # Emacs major mode for vue.js
  weyland-yutani-theme # Emacs theme based off Alien movie franchise
  wgrep # Writable grep buffer and apply the changes to files
  winum # Navigate windows and frames using numbers.
  yaml-mode # Major mode for editing YAML files
  yasnippet # Yet another snippet extension for Emacs
])
