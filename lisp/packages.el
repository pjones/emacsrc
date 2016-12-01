;;; packages.el -- Emacs package management
(require 'package)

(defvar pjones:package-archives
  '(("org"       . "http://orgmode.org/elpa/")
    ("melpa"     . "http://melpa.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/"))
  "List of package archives to use with Emacs.")

(defvar pjones:packages
  '( bbdb
     bm
     company
     company-flx
     company-ghc
     dictionary
     flycheck
     flx
     flx-ido
     gist
     haskell-mode
     htmlize
     http
     hydra
     inf-ruby
     kite
     magit
     markdown-mode
     nix-mode
     projectile
     no-littering
     rhtml-mode
     ruby-end
     scad-mode
     smex
     yaml-mode )
  "A list of packages to ensure are installed at launch.")

(dolist (archive pjones:package-archives)
  (add-to-list 'package-archives archive t))

;; Prepare package management
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Make sure all the packages are installed
(defun pjones:install-packages ()
  (dolist (p pjones:packages)
    (when (not (package-installed-p p))
      (package-install p))))
