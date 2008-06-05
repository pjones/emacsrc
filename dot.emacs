;; -*- mode: Emacs-Lisp -*-

;; Things to do when editing text files
(setq default-major-mode  'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Load pmade Custom Files
(load "~/.emacs.d/pmade/loadpath.el")
(load "~/.emacs.d/pmade/spell.el")
(load "~/.emacs.d/pmade/options.el")
(load "~/.emacs.d/pmade/packages.el")
(load "~/.emacs.d/pmade/org.el")
(load "~/.emacs.d/pmade/muse.el")
(load "~/.emacs.d/pmade/keybindings.el")
(load "~/.emacs.d/pmade/programming.el")
(load "~/.emacs.d/pmade/colors.el")
(load "~/.emacs.d/pmade/term.el")
(load "~/.emacs.d/pmade/mail.el")
(load "~/.emacs.d/pmade/server.el")

;; Stuff for the custom interface
(setq custom-file "~/.emacs.d/local/custom.el")
;; I'm not using this file right now - (load custom-file)
