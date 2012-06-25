;;; keys.el -- Global key bindings.

;; For Macintosh Emacs only
;; (setq mac-option-modifier  'hyper
;;       mac-command-modifier 'meta))

;; Global Key Bindings
(global-set-key [escape] (lambda () (interactive) (message "WTF: ESC")))
(global-set-key (kbd "C-x C-c") 'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x r S") 'string-insert-rectangle)
;; (global-set-key "\C-o"     'pmade-open-line-above)
;; (global-set-key "\M-o"     'pmade-open-line-below)
(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")   'isearch-forward)
(global-set-key (kbd "C-M-r")   'isearch-backward)
;; (global-set-key "\C-xx"    'switch-to-previous-buffer)
(global-set-key "\C-w"     'pjones:kill-region-or-backward-kill-word)
(global-set-key "\M-z"     'zap-up-to-char)
;; (global-set-key "\M-\S-z"  'zap-to-char)
;; (global-set-key "\t"       'pmade-smart-tab)
(global-set-key (kbd "C-`") 'next-error)

;; User Key Bindings (using the C-c prefix)
;; (global-set-key "\C-c;"    'flyspell-auto-correct-previous-word)
;; (global-set-key "\C-ca"    'align)
;; (global-set-key "\C-cd"    'pmade-toggle-dictionary)
;; ;                 C-c f     Toggle hide/show code
;; ;                 C-c h     Toggle header/source
;; (global-set-key "\C-cl"    'org-store-link)
;; (global-set-key "\C-cm"    'magit-status)
;; (global-set-key "\C-cn"    'pmade-insert-italian-name)
;; (global-set-key "\C-cr"    'revert-buffer)
;; (global-set-key "\C-ct"    'pmade-transpose-windows)
;; (global-set-key "\C-cu"    'goto-last-change)
;; (global-set-key "\C-c\M-w" 'save-to-kill-ring-and-normalize-whitespace)
;; ;                 C-c \t    Insert comment bar in source code file

;; ; Not using C-z right now
;; (global-set-key "\C-z"            nil)

;; ;; Super key bindings
;; (global-set-key (kbd "s-p") 'windmove-up)
;; (global-set-key (kbd "s-n") 'windmove-down)
;; (global-set-key (kbd "s-f") 'windmove-right)
;; (global-set-key (kbd "s-b") 'windmove-left)
;; (global-set-key (kbd "s-a") 'align)
;; (global-set-key (kbd "s-m") 'bm-toggle)
;; (global-set-key (kbd "s-`") 'bm-next)
;; (global-set-key (kbd "s-~") 'bm-previous)
;; (global-set-key (kbd "s-x") 'switch-to-previous-buffer)
