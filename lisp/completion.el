;;; completion.el -- Configuration for completion, abbreviations, and shortcuts.

;; I don't want to type "yes".
(defalias 'yes-or-no-p 'y-or-n-p)

;; ido mode for mini-buffer completion (see also modes/ido-conf.el)
(require 'ido)
(ido-mode t)

;; Hippie expand
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
