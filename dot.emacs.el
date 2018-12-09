;; Important load settings:
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      evil-want-integration nil          ; Needed for evil-collection.
      evil-want-keybinding nil)          ; Needed for evil-collection.

;; Prepare package management
(package-initialize)

;; Load other configuration files
(load "@loadpathel@")
(pjones:load-configuration-files)
