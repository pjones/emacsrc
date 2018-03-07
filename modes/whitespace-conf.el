;;; whitespace.el -- Configuration options for whitespace-mode.
(eval-when-compile (require 'whitespace))

(setq whitespace-style '(face tabs newline lines-tail)
      whitespace-action '(auto-cleanup))
