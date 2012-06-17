;; Things to do when editing text files
(setq default-major-mode  'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Load other configuration files
(load "~/.emacs.d/pjones/lisp/loadpath")

(dolist (file pjones:lisp-files)
  (load (concat pjones:lisp-dir file)))

;; Stuff for the custom interface (not used)
(setq custom-file "~/.emacs.d/pjones/lisp/custom.el")
