;;; embark-conf.el -- Settings for `embark' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'embark)
(require 'which-key)

(defun pjones:embark-indicator (map)
  "Show the embark MAP via `which-key'."
  (let ((which-key-side-window-location 'bottom))
    (which-key--show-keymap "Embark" map nil nil 'no-paging))
  #'which-key--hide-popup-ignore-command)

(custom-set-variables
 '(embark-prompter #'embark-keymap-prompter)
 '(embark-collect-view 'grid)
 '(embark-action-indicator #'pjones:embark-indicator)
 '(embark-become-indicator #'pjones:embark-indicator))

;; Use grids by default.
(push '(t . grid) embark-collect-initial-view-alist)

;;; embark-conf.el ends here
