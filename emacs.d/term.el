;; Settings for Terminal mode
(defun pmade-term-mode-hook ()
  (term-set-escape-char ?\C-x)
  (define-key term-raw-map "\C-c" 'term-send-raw)
  (define-key term-raw-map "\C-y" 'pmade-term-yank)
  (define-key term-raw-map "\M-y" 'yank-pop)
  (define-key term-raw-map "\M-w" 'kill-ring-save)
  (setq term-prompt-regexp "^`-->")
  (setq ansi-term-color-vector
        ;; Redefine the 8 primary terminal colors to look good against black
        [unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65" "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"]))

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