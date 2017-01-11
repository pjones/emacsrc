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
     flx
     flx-ido
     flycheck
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
     no-littering
     passmm
     projectile
     rhtml-mode
     ruby-end
     scad-mode
     smex
     yaml-mode )
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
