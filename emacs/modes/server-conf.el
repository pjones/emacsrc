;;; server-conf.el -- Settings for Emacs daemon.
(eval-when-compile (require 'server))

(setq server-use-tcp t
      server-host (cond
                   ((string= "hawkins" system-name) "10.0.1.10")
                   ((string= "seward"  system-name) "192.168.218.1")
                   (t system-name)))
