;;; keys.el -- Global key bindings.
;;; Commentary:
;;; Code:

;; For Macintosh Emacs only
;; (setq mac-option-modifier  'hyper
;;       mac-command-modifier 'meta))

;; Emacs nonstandard editing commands
(autoload 'zap-up-to-char "misc" nil t)

;; Overriding default key bindings
(global-set-key (kbd "<escape>")  'god-mode-all)
(global-set-key (kbd "C-x C-c")   'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-b")   'ibuffer)
(global-set-key (kbd "C-a")       'pjones:move-beginning-of-line)
(global-set-key (kbd "C-o")       'pjones:open-line-above)
(global-set-key (kbd "M-o")       'pjones:open-line-below)
(global-set-key (kbd "C-s")       'isearch-forward-regexp)
(global-set-key (kbd "C-r")       'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")     'isearch-forward)
(global-set-key (kbd "C-M-r")     'isearch-backward)
(global-set-key (kbd "C-w")       'pjones:kill-region-or-backward-kill-word)
(global-set-key (kbd "M-x")       'smex)
(global-set-key (kbd "M-z")       'zap-up-to-char)
(global-set-key (kbd "M-S-z")     'zap-to-char)
(global-set-key (kbd "C-`")       'next-error)
(global-set-key (kbd "M-/")       'company-complete)
(global-set-key (kbd "M-'")       'pjones:zap-to-quote)
(global-set-key (kbd "C-+")       'text-scale-increase)
(global-set-key (kbd "C--")       'text-scale-decrease)
(global-set-key (kbd "M-<tab>")   'hs-toggle-hiding)
(global-set-key (kbd "C-x C-1")   'delete-other-windows)
(global-set-key (kbd "C-x C-2")   'split-window-below)
(global-set-key (kbd "C-x C-3")   'split-window-right)
(global-set-key (kbd "C-x C-0")   'delete-window)
(global-set-key (kbd "<home>")    'beginning-of-buffer)
(global-set-key (kbd "<end>")     'end-of-buffer)

;; Remapping existing keys.
(global-set-key [remap goto-line] 'pjones:goto-line-with-feedback)

;; User Key Bindings (using the C-z prefix)
(global-set-key (kbd "C-z") (make-sparse-keymap))

(global-set-key (kbd "C-z C-SPC")   'just-one-space)
(global-set-key (kbd "C-z C-;")     'pjones:auto-correct-previous-word)
(global-set-key (kbd "C-z C-,")     'pjones:push-tag-mark)
(global-set-key (kbd "C-z C-a")     'align)
(global-set-key (kbd "C-z C-S-a")   'align-regexp)
(global-set-key (kbd "C-z C-b")     'pjones:bookmark)
(global-set-key (kbd "C-z C-c")     'quick-calc)
(global-set-key (kbd "C-z C-d")     'dictionary-lookup-definition)
(global-set-key (kbd "C-z C-S-d")   'pjones:toggle-dictionary)
(global-set-key (kbd "C-z C-e C-a") 'org-agenda)
(global-set-key (kbd "C-z C-e C-c") 'org-capture)
(global-set-key (kbd "C-z C-e C-g") 'pjones:start-primary-app)
(global-set-key (kbd "C-z C-f")     'pjones:kill-file-name)
(global-set-key (kbd "C-z C-g")     'rgrep)
;                     C-z C-h        Mode-specific hydra.
(global-set-key (kbd "C-z C-S-h")   'highline-mode)
(global-set-key (kbd "C-z C-i")     'idomenu)
(global-set-key (kbd "C-z C-S-i")   'pjones:uuid)
(global-set-key (kbd "C-z C-j")     'pjones:indium-start)
(global-set-key (kbd "C-z C-S-j")   'pjones:journal)
(global-set-key (kbd "C-z C-l")     'pjones:jump-back-and-forth)
(global-set-key (kbd "C-z C-m")     'magit-status)
(global-set-key (kbd "C-z C-n")     'pjones:insert-italian-name)
(global-set-key (kbd "C-z C-o")     'browse-url)
(global-set-key (kbd "C-z C-p")     'pjones:pwgen)
(global-set-key (kbd "C-z C-S-p")   'passmm-list-passwords)
(global-set-key (kbd "C-z C-r")     'pjones:register-get-set)
(global-set-key (kbd "C-z C-S-r")   'revert-buffer)
(global-set-key (kbd "C-z C-s")     'sort-lines)
(global-set-key (kbd "C-z C-u")     'goto-last-change)
(global-set-key (kbd "C-z C-t")     'pjones:toggle-theme)
(global-set-key (kbd "C-z C-w C-w") 'pjones:window-config)
(global-set-key (kbd "C-z C-w C-u") 'winner-undo)
(global-set-key (kbd "C-z C-w C-r") 'winner-redo)
(global-set-key (kbd "C-z C-w C-s") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-z C-w C-t") 'pjones:transpose-windows)
(global-set-key (kbd "C-z C-z")     'pjones:switch-to-previous-buffer)

(provide 'keys)
;;; keys.el ends here
