;;; packages.el -- Emacs package management
(require 'package)

(defvar pjones:package-archives
  '(("melpa" . "http://melpa.org/packages/")
    ("org"   . "http://orgmode.org/elpa/"))
  "List of package archives to use with Emacs.")

(defvar pjones:packages
  '( beginend
     bm
     circe
     circe-notifications
     color-theme-sanityinc-tomorrow
     company
     company-flx
     company-ghc
     company-quickhelp
     company-statistics
     dante
     deft
     dictionary
     dired-filter
     dired-narrow
     dired-sidebar
     dired-subtree
     flx
     flx-ido
     flycheck
     gist
     git-annex
     google-contacts
     graphviz-dot-mode
     indium
     haskell-mode
     highlight-indent-guides
     htmlize
     http
     hydra
     ido-completing-read+
     idomenu
     inf-ruby
     js2-mode
     magit
     magit-annex
     markdown-mode
     nix-mode
     nlinum
     noccur
     no-littering
     org
     org-tree-slide
     passmm
     projectile
     rainbow-mode
     ruby-end
     scad-mode
     smex
     smtpmail-multi
     yaml-mode
   )
  "A list of packages to ensure are installed at launch.")

;; Prepare package management
(package-initialize)

(dolist (archive pjones:package-archives)
  (add-to-list 'package-archives archive))

;; Make sure all the packages are installed
(defun pjones:install-packages ()
  (package-refresh-contents)
  (dolist (p pjones:packages)
    (when (not (package-installed-p p))
      (package-install p t))))
