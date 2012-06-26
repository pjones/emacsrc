;; Load other configuration files
(load "~/.emacs.d/pjones/lisp/loadpath")
(load-theme pjones:default-theme t)
(pjones:load-configuration-files)

;; Stuff for the custom interface (not used)
(setq custom-file "~/.emacs.d/pjones/lisp/custom.el")
