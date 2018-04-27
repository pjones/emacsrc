;;; server-conf.el -- Settings for Emacs daemon.
(eval-when-compile (require 'server))

(custom-set-variables
  '(server-use-tcp t)) ; Create local socket files.
