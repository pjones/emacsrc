;;; completion.el -- Configuration for completion, abbreviations, and shortcuts.
(eval-when-compile
  (require 'company)
  (require 'company-ghc))

;; I don't want to type "yes".
(defalias 'yes-or-no-p 'y-or-n-p)

;; ido mode for mini-buffer completion (see also modes/ido-conf.el)
(require 'ido)
(ido-mode t)

;; What to do with the tab key.
(setq tab-always-indent t)

;; Replace completion-at-point with hippie expand
(defun pjones:hippie-expand () (hippie-expand nil))
(defalias 'completion-at-point 'pjones:hippie-expand)

;; Hippie expand functions
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        try-expand-line))

;; In buffer completion:
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-ghc)

;; Minibuffer completion:
(ivy-mode 1)
