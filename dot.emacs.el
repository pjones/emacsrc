;; Important load settings:
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; Evil and evil-leader:
;; These need to be set before loading Evil.
(custom-set-variables
 '(evil-want-minibuffer t)
 '(evil-want-C-i-jump t)
 '(evil-want-C-u-delete t)
 '(evil-want-C-u-scroll t)
 '(evil-want-C-w-delete t)
 '(evil-want-C-w-scroll t)
 '(evil-respect-visual-line-mode t)
 '(evil-leader/leader "SPC"))

;; Prepare package management
(package-initialize)

;; Load other configuration files
(load "@loadpathel@")
(pjones:load-configuration-files)
