{
  emacs,
  emacsPackagesFor,
  inputs,
}:
let
  # Version number to use for custom packages:
  version = "99999999.0";

  # Function to update several attributes in an Emacs package:
  update =
    pkg: src:
    pkg.overrideAttrs (orig: rec {
      inherit version src;
      name = "emacs-${orig.ename or orig.pname}-${version}";

      # When updating a package, automatically remove the broken flag:
      meta = (orig.meta or { }) // {
        broken = false;
      };
    });

  # Package overrides:
  emacsWithOverrides = (emacsPackagesFor emacs).overrideScope (
    self: super: {
      link-hint = update super.link-hint inputs.link-hint;

      # Not in nixpkgs:
      nextflow-mode = emacs.pkgs.elpaBuild {
        inherit version;
        pname = "nextflow-mode";
        src = "${inputs.nextflow-mode}/nextflow-mode.el";
        packageRequires = [ super.groovy-mode ];
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

      org-inline-image-mode = emacs.pkgs.elpaBuild {
        inherit version;
        pname = "org-inline-image-mode";
        src = "${inputs.org-inline-image-mode}/org-inline-image-mode.el";
        packageRequires = [ self.org ];
      };

      ox-ipynb = emacs.pkgs.trivialBuild {
        inherit version;
        name = "ox-ipynb";
        src = inputs.ox-ipynb;
        packageRequires = [
          super.s
          super.dash
        ];
      };

      xref-project-history = emacs.pkgs.trivialBuild {
        inherit version;
        name = "xref-project-history";
        src = inputs.xref-project-history;
      };
    }
  );
in
# Emacs package list:
emacsWithOverrides.emacsWithPackages (
  epkgs: with epkgs; [
    ace-window # Quickly switch windows
    anki-editor # Emacs minor mode for making Anki cards with Org Mode
    async # Asynchronous processing in Emacs
    avy # Jump to arbitrary positions in visible text and select text quickly
    beframe # Isolate Emacs buffers per frame
    cape # Let your completions fly!
    citar # quickly find and act on bibliographic references
    citar-embark # Citar integration with embark
    citar-org-roam # citar/org-roam integration
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
    doom-themes # A megapack of themes for GNU Emacs.
    dumb-jump # Jump to definition for 40+ languages without configuration
    ef-themes # Colourful (“pretty”) yet legible themes
    eglot-signature-eldoc-talkative # Better function signatures
    eimp # Emacs Image Manipulation Package
    eldoc # Show function arglist or variable docstring in echo area
    embark # Conveniently act on minibuffer completions
    embark-consult # Consult integration for Embark
    embrace # Add/Change/Delete pairs
    empv # An Emacs media player, media library manager, radio player, YouTube frontend, Subsonic client
    envrc # Support for `direnv' that operates buffer-locally
    erc-hl-nicks # ERC nick highlighter that ignores uniquifying chars when colorizing
    ess # Emacs Speaks Statistics
    expreg # Your friendly neighborhood expand-region clone
    forge # Work with Git forges from the comfort of Magit
    git-annex # Mode for easy editing of git-annex'd files
    git-link # Emacs package to get the GitHub/Bitbucket/GitLab/... URL for a buffer location
    google-translate # Emacs interface to Google Translate
    goto-chg # Go to last change
    graphviz-dot-mode # Mode for the dot-language used by graphviz
    haskell-mode # A Haskell editing mode
    hl-todo # Highlight TODO and similar keywords
    htmlize # Convert buffer text and decorations to HTML.
    http # Yet another HTTP client
    indent-bars # Fast, configurable indentation guide-bars for Emacs
    inheritenv # Make emacs temp buffers inherit buffer-local environment variables.
    jinx # Enchanted Spell Checker
    jq-mode # Emacs major mode for editing jq queries
    js2-mode # Improved JavaScript editing mode
    json-mode # Major mode for editing JSON files
    jsonrpc # JSON-RPC library
    jupyter # An interface to communicate with Jupyter kernels.
    khardel # integrating khard, a console cardav client
    lin # Make 'hl-line-mode' more suitable for selection UIs
    link-hint # Use avy to open, copy, etc. visible links
    magit # A Git porcelain inside Emacs
    magit-annex # Control git-annex from Magit
    marginalia # Enrich existing commands with completion annotations
    markdown-mode # Major mode for Markdown-formatted text
    mastodon # Emacs client for fediverse servers that implement the Mastodon API.
    meow # Yet another modal editing on Emacs
    meow-tree-sitter # Tree-sitter powered 🌳 motions for Meow 🐱
    meson-mode # Emacs major mode for the Meson build system
    minions # A minor-mode menu for the mode line
    mu4e # Mu4e, the mu mail user agent
    nextflow-mode # Emacs major mode for Nextflow
    nix-mode # Major mode for editing .nix files
    nix-ts-mode # An Emacs major mode for editing Nix expressions
    no-littering # help keeping ~/.emacs.d clean
    noccur # Run multi-occur on project/dired files
    nov # Featureful EPUB reader mode
    ob-duckdb # Org Babel integration with DuckDB
    org # Outline-based notes management and organizer
    org-appear # Make invisible parts of Org elements appear visible.
    org-bulletproof # Automatic bullet cycling for Org mode
    org-clock-csv # Export `org-mode' clock entries to CSV format
    org-clock-dbus # Monitor org-clock from outside Emacs
    org-edna # Extensible Dependencies ’N’ Actions (EDNA) for Org Mode tasks
    org-grader # Support for grading papers in orgmode
    org-inline-image-mode # Update displayed images as an org-mode buffer changes.
    org-mime # Send HTML email using Org-mode HTML export
    org-modern # Modern Org Style.
    org-roam # A database abstraction layer for Org-mode
    org-roam-bibtex # Org Roam integration with bibliography management software
    org-tree-slide # A presentation tool for org-mode
    orgalist # Manage Org-like lists in non-Org buffers
    orgit-forge # Org links to Forge Issue buffers
    outline-indent # Folding text based on indentation
    ox-gfm # Github Flavored Markdown Back-End for Org Export Engine
    ox-ipynb # org-mode exporter to Jupyter notebooks
    package-lint # A linting library for elisp package authors
    pass # A major mode for password-store
    password-store # Password store (pass) support
    pdf-tools # Support library for PDF documents
    plantuml-mode # A major mode for editing PlantUML sources in Emacs
    prescient # Simple but effective sorting and filtering for Emacs
    project # Operations on the current project
    puni # Parentheses Universalistic
    rainbow-mode # Colorize color names in buffers
    rec-mode # Major mode for viewing/editing rec files
    reformatter # Define commands which run reformatters on the current Emacs buffer
    resize-window # easily resize windows
    rg # A search tool based on ripgrep
    rotate # Rotate the layout of emacs
    ruby-end # Automatic insertion of end blocks for Ruby
    rust-mode # A major-mode for editing Rust source code
    scad-mode # A major mode for editing OpenSCAD code
    treesit-auto # Automatic installation, usage, and fallback for tree-sitter major modes
    treesit-grammars.with-all-grammars # For use with tree-sitter
    typescript-mode # Major mode for editing typescript
    vertico # VERTical Interactive COmpletion
    vertico-prescient # Prescient support for vertico
    visual-fill # Auto-refill paragraphs without modifying the buffer
    vlf # View Large Files
    vterm # Fully-featured terminal emulator
    vue-mode # Emacs major mode for vue.js
    wgrep # Writable grep buffer and apply the changes to files
    winum # Navigate windows and frames using numbers.
    xref-project-history # Per-project xref-history-storage for Emacs
    yaml-mode # Major mode for editing YAML files
    yasnippet # Yet another snippet extension for Emacs
  ]
)
