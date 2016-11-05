;;; keys.el -- Global key bindings.
;;; Commentary:
;;; Code:

;; For Macintosh Emacs only
;; (setq mac-option-modifier  'hyper
;;       mac-command-modifier 'meta))

;; Emacs nonstandard editing commands
(autoload 'zap-up-to-char "misc" nil t)

;; Overriding default key bindings
(global-set-key [escape] (lambda () (interactive) (message "WTF: ESC")))
(global-set-key (kbd "C-x C-c") 'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x l")   'pjones:switch-to-previous-buffer)
(global-set-key (kbd "C-x m")   'magit-status)
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
(global-set-key (kbd "M-/")     'company-complete)
(global-set-key (kbd "M-SPC")   'company-complete)
(global-set-key (kbd "C-M-SPC") 'just-one-space)

;; Remapping existing keys.
(global-set-key [remap goto-line] 'pjones:goto-line-with-feedback)

;; User Key Bindings (using the C-c prefix)
(global-set-key (kbd "C-c ;")   'pjones:auto-correct-previous-word)
(global-set-key (kbd "C-c .")   'pjones:push-tag-mark)
(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c A")   'align-regexp)
(global-set-key (kbd "C-c b")   'pjones:bookmark)
(global-set-key (kbd "C-c c")   'quick-calc)
(global-set-key (kbd "C-c d")   'dictionary-lookup-definition)
(global-set-key (kbd "C-c D")   'pjones:toggle-dictionary)
(global-set-key (kbd "C-c F")   'pjones:kill-file-name)
(global-set-key (kbd "C-c g")   'pjones:project-grep-or-rgrep)
;                     C-c h      Mode-specific hydra.
(global-set-key (kbd "C-c H")   'highline-mode)
(global-set-key (kbd "C-c i")   'pjones:uuid)
(global-set-key (kbd "C-c j")   'pjones:journal)
(global-set-key (kbd "C-c k")   'pjones:kite-start)
(global-set-key (kbd "C-c l")   'pjones:jump-back-and-forth)
(global-set-key (kbd "C-c m")   'magit-status)
(global-set-key (kbd "C-c n")   'pjones:insert-italian-name)
(global-set-key (kbd "C-c p")   'pjones:pwgen)
(global-set-key (kbd "C-c P")   'pjones:password-list)
(global-set-key (kbd "C-c r")   'pjones:register-get-set)
(global-set-key (kbd "C-c R")   'revert-buffer)
(global-set-key (kbd "C-c s")   'sort-lines)
(global-set-key (kbd "C-c u")   'goto-last-change)
;                     C-c t     Insert comment bar in source code file
(global-set-key (kbd "C-c w w") 'pjones:window-config)
(global-set-key (kbd "C-c w u") 'winner-undo)
(global-set-key (kbd "C-c w r") 'winner-redo)
(global-set-key (kbd "C-c w t") 'pjones:transpose-windows)

;; C-z is used by my window manager.
(global-set-key (kbd "C-z") nil)

;; Super key bindings
(global-set-key (kbd "s-c") 'projectile-compile-project)
(global-set-key (kbd "s-t") 'projectile-test-project)
(global-set-key (kbd "s-r") 'projectile-run-project)
(global-set-key (kbd "s-g") 'pjones:start-primary-app)
(global-set-key (kbd "s-p") 'pjones:text-to-speech-para)
(global-set-key (kbd "s-u") 'browse-url)

(provide 'keys)
;;; keys.el ends here
