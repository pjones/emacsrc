;;; ace-window-conf.el -- Configure ace-window.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'ace-window))

(setq aw-scope 'frame
      aw-ignore-current t)
