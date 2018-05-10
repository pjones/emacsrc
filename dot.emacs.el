;; Important load settings:
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; Prepare package management
(package-initialize)

;; Load other configuration files
(load "@loadpathel@")
(pjones:load-configuration-files)
