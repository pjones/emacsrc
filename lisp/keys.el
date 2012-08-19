;;; keys.el -- Global key bindings.

;; For Macintosh Emacs only
;; (setq mac-option-modifier  'hyper
;;       mac-command-modifier 'meta))

;; Emacs nonstandard editing commands
(autoload 'zap-up-to-char "misc" nil t)

;; Overriding default key bindings
(global-set-key [escape] (lambda () (interactive) (message "WTF: ESC")))
(global-set-key (kbd "C-x C-c") 'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x r S") 'string-insert-rectangle)
(global-set-key (kbd "C-x l")   'pjones:switch-to-previous-buffer)
(global-set-key (kbd "C-o")     'pjones:open-line-above)
(global-set-key (kbd "M-o")     'pjones:open-line-below)
(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")   'isearch-forward)
(global-set-key (kbd "C-M-r")   'isearch-backward)
(global-set-key (kbd "C-w")     'pjones:kill-region-or-backward-kill-word)
(global-set-key (kbd "M-z")     'zap-up-to-char)
(global-set-key (kbd "M-S-z")   'zap-to-char)
(global-set-key (kbd "C-`")     'next-error)

;; User Key Bindings (using the C-c prefix)
(global-set-key (kbd "C-c ;")   'flyspell-auto-correct-previous-word)
(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c b")   'pjones:bookmark)
(global-set-key (kbd "C-c d")   'pjones:toggle-dictionary)
(global-set-key (kbd "C-c f")   'pjones:kill-file-name)
(global-set-key (kbd "C-c g")   'rgrep)
(global-set-key (kbd "C-c h")   'highline-mode)
(global-set-key (kbd "C-c j")   'pjones:journal)
(global-set-key (kbd "C-c m")   'magit-status)
(global-set-key (kbd "C-c n")   'pjones:insert-italian-name)
(global-set-key (kbd "C-c r")   'revert-buffer)
(global-set-key (kbd "C-c t")   'pjones:transpose-windows)
(global-set-key (kbd "C-c u")   'goto-last-change)
;                     C-c \t     Insert comment bar in source code file
(global-set-key (kbd "C-c w w") 'pjones:window-config)
(global-set-key (kbd "C-c w u") 'winner-undo)
(global-set-key (kbd "C-c w r") 'winner-redo)


; C-z is used by my window manager.
(global-set-key (kbd "C-z") nil)

;; Super key bindings
(global-set-key (kbd "s-A") 'align-regexp)
(global-set-key (kbd "s-a") 'align)
(global-set-key (kbd "s-b") 'windmove-left)
(global-set-key (kbd "s-f") 'windmove-right)
(global-set-key (kbd "s-g") 'gnus)
(global-set-key (kbd "s-m") 'bm-toggle)
(global-set-key (kbd "s-n") 'windmove-down)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-r") 'pjones:irc)
(global-set-key (kbd "s-u") 'browse-url)
(global-set-key (kbd "s-~") 'bm-previous)
(global-set-key (kbd "s-`") 'bm-next)
