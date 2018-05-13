;;; completion.el -- Configuration for completion, abbreviations, and shortcuts.
(require 'company)
(require 'helm)
(require 'hydra)

;; I don't want to type "yes".
(defalias 'yes-or-no-p 'y-or-n-p)

;; What to do with the tab key.
(setq tab-always-indent t)

;; In buffer completion:
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-capf)

;; Replace completion-at-point with company mode:
(defalias 'completion-at-point 'company-complete)

;; Settings for older code still using hippie expand:
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

;; Key helpers (completion):
(helm-mode)
