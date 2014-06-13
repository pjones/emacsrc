;;; packages.el -- Emacs package management
(require 'package)

(defvar pjones:package-archives
  '(("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/"))
  "List of package archives to use with Emacs.")

(defvar pjones:packages
  '(magit flymake-ruby ruby-end inf-ruby htmlize yaml-mode
          bm gist haskell-mode ido-select-window switch-window)
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
