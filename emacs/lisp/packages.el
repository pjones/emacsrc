;;; packages.el -- Emacs package management
(require 'package)

;; Which package archives to use.
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Prepare package management
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Packages I like.
(defvar pjones:packages
  '(magit flymake-ruby ruby-end inf-ruby htmlize)
  "A list of packages to ensure are installed at launch.")

;; Make sure all the packages are installed
(defun pjones:install-packages ()
  (dolist (p pjones:packages)
    (when (not (package-installed-p p))
      (package-install p))))
