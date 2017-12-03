;;; packages.el -- Emacs package management
(require 'package)

(defvar pjones:package-archives
  '(("melpa" . "http://melpa.org/packages/")
    ("org"   . "http://orgmode.org/elpa/"))
  "List of package archives to use with Emacs.")

(defvar pjones:packages
  '( bbdb
     beginend
     bm
     circe
     circe-notifications
     color-theme-sanityinc-tomorrow
     company
     company-flx
     company-ghc
     dante
     deft
     dictionary
     dired-collapse
     dired-narrow
     dired-subtree
     flx
     flx-ido
     flycheck
     gist
     google-contacts
     graphviz-dot-mode
     indium
     haskell-mode
     highlight-indent-guides
     htmlize
     http
     hydra
     ido-completing-read+
     inf-ruby
     js2-mode
     magit
     markdown-mode
     nix-mode
     no-littering
     org
     org-tree-slide
     passmm
     rainbow-mode
     rhtml-mode
     ruby-end
     scad-mode
     smex
     smtpmail-multi
     yaml-mode
     yasnippet
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
