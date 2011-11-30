;; For Macintosh Emacs only
(setq mac-option-modifier  'hyper)
(setq mac-command-modifier 'meta)

;; Global Key Bindings
(global-set-key [escape]   (lambda () (interactive) (message "WTF: ESC")))
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-xrS"   'string-insert-rectangle)
(global-set-key "\C-o"     'open-line-below-like-vim)
(global-set-key "\M-o"     'open-line-above-like-vim)
(global-set-key "\C-s"     'isearch-forward-regexp)
(global-set-key "\C-r"     'isearch-backward-regexp)
(global-set-key "\C-\M-s"  'isearch-forward)
(global-set-key "\C-\M-r"  'isearch-backward)
(global-set-key "\C-xx"    'switch-to-previous-buffer)
(global-set-key "\C-xw"    'pmade-select-window)
(global-set-key "\C-w"     'kill-region-or-backward-kill-word)
(global-set-key "\M-z"     'zap-up-to-char)
(global-set-key "\M-\S-z"  'zap-to-char)
(global-set-key "\t"       'pmade-smart-tab)

;; User Key Bindings (using the C-c prefix)
(global-set-key "\C-c;"    'flyspell-auto-correct-previous-word)
(global-set-key "\C-ca"    'align)
(global-set-key "\C-cd"    'pmade-toggle-dictionary)
;                 C-c f     Toggle hide/show code
;                 C-c h     Toggle header/source
(global-set-key "\C-cl"    'org-store-link)
(global-set-key "\C-cm"    'magit-status)
(global-set-key "\C-cr"    'revert-buffer)
(global-set-key "\C-ct"    'pmade-transpose-windows)
(global-set-key "\C-cu"    'goto-last-change)
(global-set-key "\C-c\M-w" 'save-to-kill-ring-and-normalize-whitespace)
;                 C-c \t    Insert comment bar in source code file

; Not using C-z right now
(global-set-key "\C-z"            nil)
