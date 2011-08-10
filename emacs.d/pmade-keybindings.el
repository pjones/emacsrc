;; For Macintosh Emacs only
(setq mac-option-modifier  'hyper)
(setq mac-command-modifier 'meta)

;; Global Key Bindings
(global-set-key [escape]   (lambda () (interactive) (message "WTF: ESC")))
(global-set-key "\C-x4f"   'pmade-find-file-window-1)
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
(global-set-key "\C-ca"    'org-agenda)
(global-set-key "\C-cd"    'delete-trailing-whitespace)
(global-set-key "\C-cD"    'pmade-toggle-dictionary)
;                       C-c f     Toggle hide/show code
;                       C-c h     Toggle header/source
(global-set-key "\C-cl"    'org-store-link)
(global-set-key "\C-cm"    'magit-status)
(global-set-key "\C-cr"    'revert-buffer)
(global-set-key "\C-ct"    'pmade-transpose-windows)
(global-set-key "\C-cu"    'goto-last-change)
(global-set-key "\C-c\M-w" 'save-to-kill-ring-and-normalize-whitespace)
;                       C-c \t    Insert comment bar in source code file

;; Hyper Keys
(global-set-key (kbd "H-m m") 'bm-toggle)
(global-set-key (kbd "H-m p") 'bm-previous)
(global-set-key (kbd "H-m n") 'bm-next)
(global-set-key (kbd "H-b") 'windmove-left)
(global-set-key (kbd "H-f") 'windmove-right)
(global-set-key (kbd "H-p") 'windmove-up)
(global-set-key (kbd "H-n") 'windmove-down)
(global-set-key (kbd "H-a") 'align)
(global-set-key (kbd "H-t") 'pmade-goto-terminal)
(global-set-key (kbd "H-x") 'switch-to-previous-buffer)

;; Key Bindings for Working with Windows
(global-set-key [(meta down)]     'shrink-window)              ; Make window smaller (vertical)
(global-set-key [(meta up)]       'enlarge-window)             ; Make window bigger (vertical)
(global-set-key [(meta left)]     'shrink-window-horizontally) ; Make window smaller
(global-set-key [(meta right)]    'enlarge-window-horizontally); Make window bigger
(global-set-key "\C-z"            nil)                         ; Not using C-z right now
