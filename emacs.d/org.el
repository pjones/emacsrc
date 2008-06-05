;; This file contains my customizations for the awesome Org-Mode

;; Attempt to load org-mode from a local installation, but if that
;; fails, ignore the error since an older org-mode comes with emacs.
(require 'org-install nil t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


(setq org-log-done t) ;; Record a time stamp when you mark a to-do as done
(setq org-agenda-ndays 14) ;; How many days to view in the agenda view
(setq org-deadline-warning-days 14) ;; How many days to start warning about a deadline
(setq org-agenda-skip-deadline-if-done nil)
(setq org-agenda-skip-scheduled-if-done nil)
(setq org-agenda-show-all-dates t)
(setq org-agenda-start-on-weekday 1)
(setq org-hide-leading-stars t)
(setq org-fast-tag-selection-include-todo t)
(setq org-agenda-window-setup 'current-window)

;; These remaining items are only used when I'm in a GUI
(and window-system (load "~/.emacs.d/pmade/org-full.el"))
