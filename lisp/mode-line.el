;;; mode-line.el -- Settings for the mode-line.
;;; Commentary:
;;; Code:

(require 'dim)
(require 'god-mode)

(defun pjones:mode-line-status ()
   "Indicate the status of the mode line."
  (concat
   (if god-global-mode (propertize "∄" 'face font-lock-builtin-face) "─")
   (cond
    ((or (derived-mode-p 'term-mode)
         (derived-mode-p 'comint-mode)
         (null (buffer-file-name)))
     "─")
    (buffer-read-only
     "Ø")
    ((buffer-modified-p)
     (propertize "⊃" 'face font-lock-warning-face))
    (t
     "─"))))

(setq-default mode-line-format
      '("" mode-line-front-space
        (:eval (pjones:mode-line-status))
        "   " mode-line-buffer-identification
        "   " mode-line-position
        "   " mode-line-modes
              mode-line-misc-info
              mode-line-end-spaces))

;; Display names for Major Modes:
(dim-major-names
 '((emacs-lisp-mode           "el")
   (js2-mode                  "js")))

;; Display names for Minor Modes:
(dim-minor-names
 '((auto-fill-function  " ↵" "Fill")
   (auto-revert-mode    ""   "Revert")
   (ivy-mode            ""   "ivy")
   (counsel-mode        ""   "counsel")
   (global-auto-revert-mode "" "Revert")
   (flyspell-mode       ""   "Flyspell")
   (hs-minor-mode       ""   "HS")
   (whitespace-mode     ""   "Space")))

;;; mode-line.el ends here
