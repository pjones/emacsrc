;;; completion.el -- Configuration for completion, abbreviations, and shortcuts.

;; I don't want to type "yes".
(defalias 'yes-or-no-p 'y-or-n-p)

;; ido mode for mini-buffer completion (see also modes/ido-conf.el)
(require 'ido)
(ido-mode t)
