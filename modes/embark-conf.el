;;; embark-conf.el -- Settings for `embark' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'embark)
(require 'embark-consult)

;; Partially stolen from:
;; https://protesilaos.com/dotemacs/
(defun pjones:embark-collect-fit-window ()
  "Hook to resize the `embark-collect-completions' window."
  (when-let* ((buffer (current-buffer))
              (name (buffer-name buffer))
              (window (get-buffer-window buffer)))
    (when (string-match-p "Embark Collect \\(Live\\|Completions\\)" name)
      (fit-window-to-buffer window (floor (frame-height) 2) 1))))

(custom-set-variables
 '(embark-indicators '(embark-highlight-indicator
                       embark-isearch-highlight-indicator))
 '(embark-prompter #'embark-keymap-prompter)
 '(embark-collect-view 'grid)
 '(embark-action-indicator #'pjones:embark-indicator)
 '(embark-become-indicator #'pjones:embark-indicator)
 '(embark-collect-initial-view-alist '((t . grid))))

(add-hook
 'embark-collect-post-revert-hook
 #'pjones:embark-collect-fit-window)

;;; embark-conf.el ends here
