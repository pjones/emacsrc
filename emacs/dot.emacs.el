;; Things to do when editing text files
(setq default-major-mode  'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Load pmade Custom Files
(load "~/.emacs.d/pmade/pmade-loadpath")
(load "~/.emacs.d/pmade/pmade-options")
(load "~/.emacs.d/pmade/pmade-packages")
(load "~/.emacs.d/pmade/pmade-muse")
(load "~/.emacs.d/pmade/pmade-keybindings")
(load "~/.emacs.d/pmade/pmade-programming")
(load "~/.emacs.d/pmade/pmade-colors")
(load "~/.emacs.d/pmade/pmade-term")
(load "~/.emacs.d/pmade/pmade-server")
(load "~/.emacs.d/pmade/pmade-local")

;; Stuff for the custom interface (not used)
(setq custom-file "~/.emacs.d/pmade/pmade-custom.el")
