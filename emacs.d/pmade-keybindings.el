;; For Macintosh Emacs only
(setq mac-option-modifier  'hyper)
(setq mac-command-modifier 'meta)

;; Global Key Bindings
(define-key global-map [escape]   (lambda () (interactive) (message "WTF: ESC")))
(define-key global-map "\C-x\C-m" 'execute-extended-command)
(define-key global-map "\C-x\C-b" 'ibuffer)
(define-key global-map "\C-o"     'open-line-below-like-vim)
(define-key global-map "\M-o"     'open-line-above-like-vim)
(define-key global-map "\C-s"     'isearch-forward-regexp)
(define-key global-map "\C-r"     'isearch-backward-regexp)
(define-key global-map "\C-\M-s"  'isearch-forward)
(define-key global-map "\C-\M-r"  'isearch-backward)
(define-key global-map "\C-xx"    'switch-to-previous-buffer)
(define-key global-map "\C-xw"    'pmade-select-window)
(define-key global-map "\C-w"     'kill-region-or-backward-kill-word)
(define-key global-map "\M-z"     'zap-up-to-char)
(define-key global-map "\M-\S-z"  'zap-to-char)
(define-key global-map "\t"       'pmade-smart-tab)

;; User Key Bindings (using the C-c prefix)
(define-key global-map "\C-ca"    'org-agenda)
(define-key global-map "\C-cd"    'delete-trailing-whitespace)
;                       C-c f     Toggle hide/show code
;                       C-c h     Toggle header/source
(define-key global-map "\C-cl"    'org-store-link)
(define-key global-map "\C-cm"    'magit-status)
(define-key global-map "\C-cr"    'revert-buffer)
(define-key global-map "\C-ct"    'pmade-transpose-windows)
(define-key global-map "\C-cu"    'goto-last-change)
(define-key global-map "\C-c\M-w" 'save-to-kill-ring-and-normalize-whitespace)
(define-key global-map "\C-c3"    'pmade-3-windows)
;                       C-c \t    Insert comment bar in source code file

;; Hyper Keys
(define-key global-map (kbd "H-m") 'bm-toggle)
;; (define-key global-map (kbd "H-p") 'bm-previous)
;; (define-key global-map (kbd "H-n") 'bm-next)
(define-key global-map (kbd "H-b") 'windmove-left)
(define-key global-map (kbd "H-f") 'windmove-right)
(define-key global-map (kbd "H-p") 'windmove-up)
(define-key global-map (kbd "H-n") 'windmove-down)
(define-key global-map (kbd "H-a") 'align)
(define-key global-map (kbd "H-s") 'pmade-split-frame)
(define-key global-map (kbd "H-t") 'pmade-goto-terminal)
(define-key global-map (kbd "H-x") 'switch-to-previous-buffer)

;; Key Bindings for Working with Windows
(define-key global-map [(meta down)]     'shrink-window)              ; Make window smaller (vertical)
(define-key global-map [(meta up)]       'enlarge-window)             ; Make window bigger (vertical)
(define-key global-map [(meta left)]     'shrink-window-horizontally) ; Make window smaller
(define-key global-map [(meta right)]    'enlarge-window-horizontally); Make window bigger

;; I freaking hate C-z (unless I'm using escreen)
(if (fboundp 'escreen-create-screen) 
    (progn
      (setq escreen-prefix-char "\C-z")
      (global-set-key escreen-prefix-char 'escreen-prefix)
      (global-set-key "\C-z\C-z" 'escreen-goto-last-screen)
      (global-set-key "\C-\zl" 'pmade:escreen-get-active-screen-numbers-with-emphasis))
  (define-key global-map "\C-z" nil))

