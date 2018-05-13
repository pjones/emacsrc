;;; mode-line.el -- Settings for the mode-line.

(require 'dim)

;; Display names for Major Modes:
(dim-major-names
 '((emacs-lisp-mode           "el")
   (js2-mode                  "js")))

;; Display names for Minor Modes:
(dim-minor-names
 '((auto-fill-function  " ↵" "Fill")
   (auto-revert-mode    ""   "Revert")
   (helm-mode           ""   "Helm")
   (global-auto-revert-mode "" "Revert")
   (flyspell-mode       ""   "Flyspell")
   (hs-minor-mode       ""   "HS")
   (whitespace-mode     ""   "Space")))
