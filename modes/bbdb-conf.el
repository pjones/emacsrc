;;; bbdb-conf.el -- Big Brother Database config.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'bbdb))

(setq
 ; Cycling while completing email addresses:
 bbdb-complete-mail-allow-cycling t

 ; Currently a remote server is the master:
 bbdb-read-only t
 bbdb-auto-revert t

 ; No popup-buffers:
 bbdb-pop-up-window-size 0
 bbdb-pop-up-layout 'one-line
 bbdb-mua-pop-up nil)
