{ lib
, emacs
, emacsPackagesFor
, notmuch
}:
let
  # Package sources:
  sources = import ./sources.nix;

  # Function to update several attributes in an Emacs package:
  update = pkg: src: pkg.overrideAttrs (orig: rec {
    inherit src;

    name = "emacs-${orig.ename or orig.pname}-${version}";
    version = "99999999.0000";

    # When updating a package, automatically remove the broken flag:
    meta = (orig.meta or { }) // { broken = false; };
  });

  # Package overrides:
  emacsWithOverrides = (emacsPackagesFor emacs).overrideScope' (self: super: {
    connection = update super.connection sources.dictionary-el;
    dictionary = update super.dictionary sources.dictionary-el;
    link = update super.link sources.dictionary-el;

    # https://github.com/felko/neuron-mode/issues/76
    neuron-mode = update super.neuron-mode sources.neuron-mode;

    goto-chg = update super.goto-chg sources.goto-chg;
    passmm = update super.passmm sources.passmm;
  });

in
# Emacs package list:
emacsWithOverrides.emacsWithPackages (epkgs:
  with epkgs; [
    ace-window # Quickly switch windows
    adaptive-wrap # Smart line-wrapping with wrap-prefix
    async # Asynchronous processing in Emacs
    avy # Jump to arbitrary positions in visible text and select text quickly
    clojure-mode # Major mode for Clojure code
    company # Modular text completion framework
    company-quickhelp # Popup documentation for completion candidates
    consult # Consulting completing-read
    csv-mode # Major mode for editing comma/char separated values
    darkroom # Remove visual distractions and focus on writing
    default-text-scale # Easily adjust the font size in all frames
    dictionary # Client for rfc2229 dictionary servers
    diff-hl # Highlight uncommitted changes using VC
    dired-filter # Ibuffer-like filtering for dired
    dired-narrow # Live-narrowing of search results for dired
    dired-subtree # Insert subdirectories in a tree-like fashion
    direnv # direnv support
    doom-modeline # A minimal and modern mode-line
    doom-themes # an opinionated pack of modern color-themes
    dumb-jump # Jump to definition for 40+ languages without configuration
    eglot # Client for Language Server Protocol (LSP) servers
    eimp # Emacs Image Manipulation Package
    eldoc # Show function arglist or variable docstring in echo area
    elm-mode # Elm mode for emacs
    embark # Conveniently act on minibuffer completions
    erc-hl-nicks # ERC nick highlighter that ignores uniquifying chars when colorizing
    expand-region # Increase selected region by semantic units.
    flycheck # On-the-fly syntax checking
    flyspell-correct # Correcting words with flyspell via custom interface
    flyspell-correct-avy-menu # Correcting words with flyspell via avy-menu interface
    forge # Access Git forges from Magit
    git-annex # Mode for easy editing of git-annex'd files
    go-mode # Major mode for the Go programming language
    goto-chg # Go to last change
    graphviz-dot-mode # Mode for the dot-language used by graphviz
    haskell-mode # A Haskell editing mode
    hasky-extensions # Toggle Haskell language extensions
    highlight-indent-guides # Minor mode to highlight indentation
    http # Yet another HTTP client
    indium # JavaScript Awesome Development Environment
    inf-ruby # Run a Ruby process in a buffer
    js2-mode # Improved JavaScript editing mode
    json-mode # Major mode for editing JSON files
    jsonrpc # JSON-RPC library
    link-hint # Use avy to open, copy, etc. visible links
    magit # A Git porcelain inside Emacs
    magit-annex # Control git-annex from Magit
    marginalia # Enrich existing commands with completion annotations
    markdown-mode # Major mode for Markdown-formatted text
    minions # A minor-mode menu for the mode line
    neuron-mode # Major mode for editing zettelkasten notes using neuron
    nix-mode # Major mode for editing .nix files
    no-littering # help keeping ~/.emacs.d clean
    noccur # Run multi-occur on project/dired files
    notmuch.emacs # Notmunch emacs library.
    nov # Featureful EPUB reader mode
    org # Outline-based notes management and organizer
    org-bullets # Show bullets in org-mode as UTF-8 characters
    org-clock-csv # Export `org-mode' clock entries to CSV format
    org-tree-slide # A presentation tool for org-mode
    orgalist # Manage Org-like lists in non-Org buffers
    origami # Flexible text folding
    passmm # A minor mode for pass (Password Store).
    password-store # Password store (pass) support
    pdf-tools # Support library for PDF documents
    poly-erb # Polymode for erb
    poly-markdown # Polymode for markdown-mode
    polymode # Extensible framework for multiple major modes
    prescient # Better sorting and filtering
    project # Operations on the current project
    projectile # Manage and navigate projects in Emacs easily
    purescript-mode # A PureScript editing mode
    rainbow-mode # Colorize color names in buffers
    reformatter # Define commands which run reformatters on the current Emacs buffer
    resize-window # easily resize windows
    rg # A search tool based on ripgrep
    rotate # Rotate the layout of emacs
    rust-mode # A major-mode for editing Rust source code
    scad-mode # A major mode for editing OpenSCAD code
    selectrum # Easily select item from list
    selectrum-prescient # Selectrum integration
    smartparens # Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
    typescript-mode # Major mode for editing typescript
    visual-fill # Auto-refill paragraphs without modifying the buffer
    vlf # View Large Files
    weyland-yutani-theme # Emacs theme based off Alien movie franchise
    wgrep # Writable grep buffer and apply the changes to files
    which-key # Display available keybindings in popup
    winum # Navigate windows and frames using numbers.
    yaml-mode # Major mode for editing YAML files
    yasnippet # Yet another snippet extension for Emacs
  ])
