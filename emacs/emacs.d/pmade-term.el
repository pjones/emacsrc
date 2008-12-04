(eval-when-compile
  (require 'term))

;; Settings for Terminal mode
(defun pmade-term-mode-hook ()
  ;; Window Number Mode F**ks up C-x C-j
  (when (fboundp 'window-number-select)
    (dotimes (i 10)
      (define-key window-number-mode-map 
        (concat "\C-x\C-j" (number-to-string i)) nil))
    (define-key window-number-mode-map "\C-x\C-j" nil))
  
  ;; Key Bindings (C-x is the prefix command)
  (term-set-escape-char ?\C-x)
  
  ;; C-x C-j toggles between char-mode and line-mode
  (define-key term-raw-escape-map "\C-j" 'pmade-term-toggle-mode)
  (define-key term-mode-map "\C-x\C-j" 'pmade-term-toggle-mode)
  
  ;; C-z is used for elscreen, C-x C-z send C-z to terminal
  (define-key term-raw-escape-map "\C-z" 'term-send-raw)
  (when (fboundp 'elscreen-create) (define-key term-raw-map "\C-z" elscreen-map))

  ;; C-x C-f opens a file in other-window
  (define-key term-raw-map "\C-x\C-f" 'ido-find-file-other-window)
  
  ;; Some other nice bindings
  (define-key term-raw-map "\C-y" 'pmade-term-yank)
  (define-key term-raw-map "\M-y" 'yank-pop)
  (define-key term-raw-map "\M-w" 'kill-ring-save)
  (define-key term-raw-map "\M-:" 'eval-expression)
  
  (setq term-prompt-regexp "^`--> ")
  
  ;; Redefine the 8 primary terminal colors to look good against black
  (setq ansi-term-color-vector
        [unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65" 
                     "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"]))

(defun pmade-term-toggle-mode ()
  "Toggle between term-char-mode and term-line-mode."
  (interactive)
  (if (term-in-line-mode) (term-char-mode) (term-line-mode)))

(defun pmade-term-yank ()
  "Allow yank to work in raw char mode"
  (interactive)
  (term-line-mode)
  (yank)
  (term-char-mode))

(add-hook 'term-mode-hook 'pmade-term-mode-hook)

;; Settings for all comint based modes
(defun pmade-comint-mode-hook()
  (setq comint-process-echoes t)
  (ansi-color-for-comint-mode-on))

(add-hook 'comint-mode-hook 'pmade-comint-mode-hook)
