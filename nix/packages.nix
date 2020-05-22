{ sources ? import ./sources.nix, pkgs ? import sources.nixpkgs { }, emacs }:

let
  # Package overrides:
  overrides = (pkgs.emacsPackagesFor emacs).overrideScope' (self: super: rec {
    passmm = super.passmm.overrideAttrs (_: { src = sources.passmm; });
    eglot = super.eglot.overrideAttrs (_: { src = sources.eglot; });
    elfeed = super.elfeed.overrideAttrs (_: { src = sources.elfeed; });
    evil = super.evil.overrideAttrs (_: { src = sources.evil; });
    evil-collection = super.evil-collection.overrideAttrs
      (_: { src = sources.evil-collection; });
    evil-indent-textobject = super.evil-indent-textobject.overrideAttrs
      (_: { src = sources.evil-indent-textobject; });
    org-trello =
      super.org-trello.overrideAttrs (_: { src = sources.org-trello; });
    reformatter =
      super.reformatter.overrideAttrs (_: { src = sources."reformatter.el"; });
    treemacs = super.treemacs.overrideAttrs (_: { src = sources.treemacs; });
    treemacs-evil =
      super.treemacs-evil.overrideAttrs (_: { src = sources.treemacs; });
    treemacs-projectile =
      super.treemacs-projectile.overrideAttrs (_: { src = sources.treemacs; });

    # Not yet in nixpkgs:
    org-roam = super.melpaBuild {
      pname = "org-roam";
      version = pkgs.lib.removePrefix "v" sources.org-roam.branch;
      src = sources.org-roam;
      recipe = pkgs.writeText "org-roam-recipe" ''
        (org-roam :fetcher github
          :repo "org-roam/org-roam")
      '';
    };
    company-org-roam = super.melpaBuild {
      pname = "company-org-roam";
      version = pkgs.lib.removePrefix "v" sources.company-org-roam.branch;
      src = sources.company-org-roam;
      recipe = pkgs.writeText "company-org-roam-recipe" ''
        (company-org-roam :fetcher github
          :repo "org-roam/company-org-roam")
      '';
    };
  });

  # Emacs package list:
in overrides.emacsWithPackages (epkgs:
  with epkgs; [
    org-roam # Roam Research replica with Org-mode
    company-org-roam # Company backend for Org-roam
    org-journal # a simple org-mode based journaling mode

    adaptive-wrap # Smart line-wrapping with wrap-prefix
    async # Asynchronous processing in Emacs
    beginend # Redefine M-< and M-> for some modes
    company # Modular text completion framework
    company-quickhelp # Popup documentation for completion candidates
    company-statistics # Sort candidates using completion history
    counsel # Various completion functions using Ivy
    counsel-world-clock # Display world clock using Ivy
    csv-mode # Major mode for editing comma/char separated values
    cyberpunk-theme # Cyberpunk Color Theme
    darkroom # Remove visual distractions and focus on writing
    deadgrep # fast, friendly searching with ripgrep
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
    edit-server # server that responds to edit requests from Chrome
    eglot # Client for Language Server Protocol (LSP) servers
    eimp # Emacs Image Manipulation Package
    elfeed # an Emacs Atom/RSS feed reader
    elfeed-protocol # Provide fever/newsblur/owncloud/ttrss protocols for elfeed
    elm-mode # Elm mode for emacs
    erc-hl-nicks # ERC nick highlighter that ignores uniquifying chars when colorizing
    evil # Extensible Vi layer for Emacs
    evil-collection # A set of keybindings for Evil mode
    evil-commentary # Comment stuff out. A port of vim-commentary
    evil-fringe-mark # Display evil-mode marks in the fringe
    evil-indent-textobject # evil textobjects based on indentation
    evil-leader # let there be <leader>
    evil-magit # evil-based key bindings for magit
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
    kaolin-themes # A set of eye pleasing themes
    magit # A Git porcelain inside Emacs
    magit-annex # Control git-annex from Magit
    markdown-mode # Major mode for Markdown-formatted text
    minions # A minor-mode menu for the mode line
    monokai-theme # A fruity color theme for Emacs
    nix-mode # Major mode for editing .nix files
    no-littering # help keeping ~/.emacs.d clean
    noccur # Run multi-occur on project/dired files
    nov # Featureful EPUB reader mode
    org # Outline-based notes management and organizer
    org-bullets # Show bullets in org-mode as UTF-8 characters
    org-clock-csv # Export `org-mode' clock entries to CSV format
    org-mru-clock # clock in/out of tasks with completion and persistent history
    org-tree-slide # A presentation tool for org-mode
    org-trello # Minor mode to synchronize org-mode buffer and trello board
    orgalist # Manage Org-like lists in non-Org buffers
    passmm # A minor mode for pass (Password Store).
    password-store # Password store (pass) support
    pdf-tools # Support library for PDF documents
    pkgs.mu # maildir indexer/searcher + emacs mail client
    poly-erb # Polymode for erb
    poly-markdown # Polymode for markdown-mode
    polymode # Extensible framework for multiple major modes
    projectile # Manage and navigate projects in Emacs easily
    purescript-mode # A PureScript editing mode
    rainbow-mode # Colorize color names in buffers
    reformatter # Define commands which run reformatters on the current Emacs buffer
    resize-window # easily resize windows
    ruby-end # Automatic insertion of end blocks for Ruby
    scad-mode # A major mode for editing OpenSCAD code
    shackle # Enforce rules for popups
    smex # M-x interface with Ido-style fuzzy matching
    switch-window # A *visual* way to switch window
    treemacs # A tree style file explorer package
    treemacs-evil # Evil mode integration for treemacs
    treemacs-projectile # Projectile integration for treemacs
    typescript-mode # Major mode for editing typescript
    visual-fill # Auto-refill paragraphs without modifying the buffer
    vlf # View Large Files
    wgrep # Writable grep buffer and apply the changes to files
    which-key # Display available keybindings in popup
    yaml-mode # Major mode for editing YAML files
    yasnippet # Yet another snippet extension for Emacs
  ])
