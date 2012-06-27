;; Load other configuration files
(load (concat user-emacs-directory "pjones/lisp/loadpath"))
(load-theme pjones:default-theme t)
(pjones:load-configuration-files)
