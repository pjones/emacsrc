{ lib
, fetchurl
, writeText
, emacs
, emacsPackagesFor
, notmuch
}:
let
  # Package sources:
  sources = import ./sources.nix;

  # Latest versions of existing packages (or packages not in nixpkgs):
  latest = {
    connection = sources.dictionary-el;
    dictionary = sources.dictionary-el;
    link = sources.dictionary-el;

    doom-themes = sources.emacs-doom-themes;
    eglot = sources.eglot;
    evil = sources.evil;
    evil-indent-textobject = sources.evil-indent-textobject;
    neuron-mode = sources.neuron-mode;
    origami = sources."origami.el";
    passmm = sources.passmm;
    reformatter = sources."reformatter.el";
    treemacs = sources.treemacs;
    treemacs-evil = sources.treemacs;
    treemacs-projectile = sources.treemacs;

    flycheck = self: super: {
      src = sources.flycheck;

      # Patch for hlint 3 support:
      # https://github.com/flycheck/flycheck/pull/1804
      patches = [
        (fetchurl {
          url = "https://github.com/flycheck/flycheck/commit/5ddcd0fe76fb7753c694e542082ae884e7f1227a.diff";
          sha256 = "0bbhm263m0hbgiwn6b7ns92qjabf3sak11q3hj4lznqdzjfkfd8m";
        })
      ];
    };

    eldoc = self: super: rec {
      version = "1.9.0";
      src = fetchurl {
        url = "http://elpa.gnu.org/packages/eldoc-${version}.el";
        sha256 = "0y6xrf1m4949d9k5mbqhb1w6zga5k71zazvbay6af70gz8k447cg";
      };
    };
  };

  # A function that can override packages using the `latest' attrset.
  overrideFromLatest = self: super:
    let
      drv = orig: value:
        if builtins.isFunction value
        then value self super
        else {
          src = value;

          # When updating a package, automatically remove the broken flag:
          meta = (orig.meta or { }) // { broken = false; };
        };
    in
    lib.mapAttrs
      (name: value:
        if super ? ${name}
        then super.${name}.overrideAttrs (orig: drv orig value)
        else drv null value)
      latest;

  # Package overrides:
  overrides = (emacsPackagesFor emacs).overrideScope' overrideFromLatest;

in
# Emacs package list:
overrides.emacsWithPackages (epkgs:
  with epkgs; [
    adaptive-wrap # Smart line-wrapping with wrap-prefix
    async # Asynchronous processing in Emacs
    avy # Jump to arbitrary positions in visible text and select text quickly
    beginend # Redefine M-< and M-> for some modes
    company # Modular text completion framework
    company-quickhelp # Popup documentation for completion candidates
    company-statistics # Sort candidates using completion history
    counsel # Various completion functions using Ivy
    counsel-notmuch # Search emails in Notmuch asynchronously with Ivy
    counsel-world-clock # Display world clock using Ivy
    csv-mode # Major mode for editing comma/char separated values
    darkroom # Remove visual distractions and focus on writing
    default-text-scale # Easily adjust the font size in all frames
    deft # quickly browse, filter, and edit plain text notes
    dianyou # Search and analyze mails in Gnus
    dictionary # Client for rfc2229 dictionary servers
    diff-hl # Highlight uncommitted changes using VC
    dired-filter # Ibuffer-like filtering for dired
    dired-narrow # Live-narrowing of search results for dired
    dired-subtree # Insert subdirectories in a tree-like fashion
    direnv # direnv support
    doom-modeline # A minimal and modern mode-line
    doom-themes # an opinionated pack of modern color-themes
    eglot # Client for Language Server Protocol (LSP) servers
    eimp # Emacs Image Manipulation Package
    eldoc # Show function arglist or variable docstring in echo area
    elm-mode # Elm mode for emacs
    erc-hl-nicks # ERC nick highlighter that ignores uniquifying chars when colorizing
    evil # Extensible Vi layer for Emacs
    evil-commentary # Comment stuff out. A port of vim-commentary
    evil-fringe-mark # Display evil-mode marks in the fringe
    evil-indent-textobject # evil textobjects based on indentation
    evil-leader # let there be <leader>
    evil-matchit # Vim matchit ported to Evil
    evil-org # evil keybindings for org-mode
    evil-owl # Preview evil registers and marks before using them
    evil-surround # emulate surround.vim from Vim
    evil-textobj-syntax # Provides syntax text objects
    flycheck # On-the-fly syntax checking
    flymake-hlint # A flymake handler for haskell-mode files using hlint
    flyspell-correct # Correcting words with flyspell via custom interface
    flyspell-correct-ivy # Correcting words with flyspell via ivy interface
    forge # Access Git forges from Magit
    git-annex # Mode for easy editing of git-annex'd files
    go-mode # Major mode for the Go programming language
    google-contacts # Support for Google Contacts in Emacs
    graphviz-dot-mode # Mode for the dot-language used by graphviz
    haskell-mode # A Haskell editing mode
    hasky-extensions # Toggle Haskell language extensions
    highlight-indent-guides # Minor mode to highlight indentation
    htmlize # Convert buffer text and decorations to HTML
    http # Yet another HTTP client
    ialign # visual align-regexp
    indium # JavaScript Awesome Development Environment
    inf-ruby # Run a Ruby process in a buffer
    ivy # Incremental Vertical completion
    ivy-rich # More friendly display transformer for ivy
    js2-mode # Improved JavaScript editing mode
    json-mode # Major mode for editing JSON files
    jsonrpc # JSON-RPC library
    link-hint # Use avy to open, copy, etc. visible links
    magit # A Git porcelain inside Emacs
    magit-annex # Control git-annex from Magit
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
    org-mru-clock # clock in/out of tasks with completion and persistent history
    org-tree-slide # A presentation tool for org-mode
    orgalist # Manage Org-like lists in non-Org buffers
    origami # Flexible text folding
    passmm # A minor mode for pass (Password Store).
    password-store # Password store (pass) support
    pdf-tools # Support library for PDF documents
    poly-erb # Polymode for erb
    poly-markdown # Polymode for markdown-mode
    polymode # Extensible framework for multiple major modes
    project # Operations on the current project
    projectile # Manage and navigate projects in Emacs easily
    purescript-mode # A PureScript editing mode
    rainbow-mode # Colorize color names in buffers
    reformatter # Define commands which run reformatters on the current Emacs buffer
    resize-window # easily resize windows
    rg # A search tool based on ripgrep
    ruby-end # Automatic insertion of end blocks for Ruby
    scad-mode # A major mode for editing OpenSCAD code
    shackle # Enforce rules for popups
    smartrep # Support sequential operation which omitted prefix keys
    smex # M-x interface with Ido-style fuzzy matching
    sweet-theme # Sweet-looking theme
    switch-window # A *visual* way to switch window
    treemacs # A tree style file explorer package
    treemacs-evil # Evil mode integration for treemacs
    treemacs-projectile # Projectile integration for treemacs
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
