;; Important load settings:
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; Load other configuration files
(load "@loadpathel@")
(pjones:load-configuration-files)
