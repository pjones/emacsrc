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

  # Build a MELPA package that isn't in nixpkgs:
  melpa = name: src: args: super:
    super.melpaBuild
      {
        inherit src;
        pname = name;
        version = "20210108.1"; # Dummy version number.
        recipe = "${sources.melpa}/recipes/${name}";
      } // args;


  # Latest versions of existing packages (or packages not in nixpkgs):
  latest = {
    connection = sources.dictionary-el;
    dictionary = sources.dictionary-el;
    link = sources.dictionary-el;

    prescient = sources."prescient.el";
    selectrum-prescient = sources."prescient.el";
    company-prescient = sources."prescient.el";

    consult = self: melpa "consult" sources.consult { };
    eglot = sources.eglot;
    embark = self: melpa "embark" sources.embark { };
    marginalia = self: melpa "marginalia" sources.marginalia { };
    neuron-mode = sources.neuron-mode;
    origami = sources."origami.el";
    passmm = sources.passmm;
    reformatter = sources."reformatter.el";
    selectrum = sources.selectrum;

    eldoc = self: super: rec {
      version = "1.11.0";
      src = fetchurl {
        url = "http://elpa.gnu.org/packages/eldoc-${version}.el";
        sha256 = "1py9l1vl7s90y5kfpglhy11jswam2gcrqap09h6wb5ldnyb8cgq2";
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
    ace-window # Quickly switch windows
    adaptive-wrap # Smart line-wrapping with wrap-prefix
    async # Asynchronous processing in Emacs
    avy # Jump to arbitrary positions in visible text and select text quickly
    color-theme-sanityinc-tomorrow # A version of Chris Kempson's "tomorrow" themes
    company # Modular text completion framework
    company-prescient # prescient.el + Company
    company-quickhelp # Popup documentation for completion candidates
    company-try-hard # get all completions from company backends
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
    monokai-theme # A fruity color theme for Emacs.
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
    ruby-end # Automatic insertion of end blocks for Ruby
    scad-mode # A major mode for editing OpenSCAD code
    selectrum # Easily select item from list
    selectrum-prescient # Selectrum integration
    sweet-theme # Sweet-looking theme
    switch-window # A *visual* way to switch window
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
