;; Things to do when editing text files
(setq default-major-mode  'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Load pmade Custom Files
(load "~/.emacs.d/pmade/loadpath")
(load "~/.emacs.d/pmade/spell")
(load "~/.emacs.d/pmade/options")
(load "~/.emacs.d/pmade/packages")
(load "~/.emacs.d/pmade/org")
(load "~/.emacs.d/pmade/muse")
(load "~/.emacs.d/pmade/keybindings")
(load "~/.emacs.d/pmade/programming")
(load "~/.emacs.d/pmade/colors")
(load "~/.emacs.d/pmade/term")
(load "~/.emacs.d/pmade/mail")
(load "~/.emacs.d/pmade/server")

;; Stuff for the custom interface (not used)
(setq custom-file "~/.emacs.d/pmade/custom.el")
