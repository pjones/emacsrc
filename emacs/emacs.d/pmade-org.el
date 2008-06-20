;; This file contains my customizations for the awesome Org-Mode

;; Attempt to load org-mode from a local installation, but if that
;; fails, ignore the error since an older org-mode comes with emacs.
(require 'org-install nil t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq
 org-log-done t
 org-agenda-ndays 14
 org-deadline-warning-days 14
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-show-all-dates t
 org-agenda-start-on-weekday 1
 org-hide-leading-stars t
 org-use-fast-todo-selection t
 org-special-ctrl-a/e t
 org-special-ctrl-k t
 org-M-RET-may-split-line nil
 org-time-clocksum-format "%02d:%02d"
 org-agenda-window-setup 'current-window)

;; These remaining items are only used when I'm in a GUI
(and window-system (load "~/.emacs.d/pmade/pmade-org-full"))
