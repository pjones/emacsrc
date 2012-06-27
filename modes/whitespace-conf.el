;;; whitespace.el -- Configuration options for whitespace-mode.
(eval-when-compile (require 'whitespace))

(setq whitespace-style '(face trailing tabs newline lines-tail empty)
      whitespace-action '(auto-cleanup))
