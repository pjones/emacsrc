(eval-when-compile
  (require 'term))

;; Fix a bug in term.el
(eval-after-load "term"
  '(defun term-handle-ansi-terminal-messages (message)
    (if (null term-ansi-at-dir) (setq term-ansi-at-dir default-directory))
    (if (null term-ansi-at-host) (setq term-ansi-at-host (system-name)))
    (if (null term-ansi-at-user) (setq term-ansi-at-user (user-real-login-name)))
    
    (let ((updated))
      ;; Is there a command here?
      (while (string-match "\eAnSiT.+\n" message)
        ;; Extract the command code and the argument.
        (let* ((start (match-beginning 0))
               (command-code (aref message (+ start 6)))
               (argument
                (save-match-data
                  (substring message
                             (+ start 8)
                             (string-match "\r?\n" message
                                           (+ start 8))))))
          ;; Delete this command from MESSAGE.
          (setq message (replace-match "" t t message))

          ;; If we recognize the type of command, set the appropriate variable.
          (cond ((= command-code ?c)
                 (setq term-ansi-at-dir argument
                       updated t))
                ((= command-code ?h)
                 (setq term-ansi-at-host argument
                       updated t))
                ((= command-code ?u)
                 (setq term-ansi-at-user argument
                       updated t)))))

      ;; Update default-directory based on the changes this command made.
      (if updated
          (setq default-directory
                (file-name-as-directory
                 (if (and (string= term-ansi-at-host (system-name))
                          (string= term-ansi-at-user (user-real-login-name)))
                     (expand-file-name term-ansi-at-dir)
                   (if (string= term-ansi-at-user (user-real-login-name))
                       (concat "/" term-ansi-at-host ":" term-ansi-at-dir)
                     (concat "/" term-ansi-at-user "@" term-ansi-at-host ":"
                             term-ansi-at-dir))))))

      ;; I'm not sure this is necessary,
      ;; but it's best to be on the safe side.
      (if (string= term-ansi-at-host (system-name))
          (progn
            (setq ange-ftp-default-user term-ansi-at-save-user)
            (setq ange-ftp-default-password term-ansi-at-save-pwd)
            (setq ange-ftp-generate-anonymous-password term-ansi-at-save-anon))
        (setq term-ansi-at-save-user ange-ftp-default-user)
        (setq term-ansi-at-save-pwd ange-ftp-default-password)
        (setq term-ansi-at-save-anon ange-ftp-generate-anonymous-password)
        (setq ange-ftp-default-user nil)
        (setq ange-ftp-default-password nil)
        (setq ange-ftp-generate-anonymous-password nil))
      message)))
  
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
  
  ;; C-z is used for escreen, C-x C-z send C-z to terminal
  (define-key term-raw-escape-map "\C-z" 'term-send-raw)
  (when (fboundp 'escreen-create-screen) (define-key term-raw-map "\C-z" escreen-map))

  ;; C-x C-f opens a file in other-window
  (define-key term-raw-map "\C-x\C-f" 'pmade-find-file-code-window)
  
  ;; Some other nice bindings
  (define-key term-raw-map [escape] (lambda () (interactive) (term-send-raw-string "")))
  (define-key term-raw-map "\C-y" 'pmade-term-yank)
  (define-key term-raw-map "\M-y" 'yank-pop)
  (define-key term-raw-map "\M-w" 'kill-ring-save)
  (define-key term-raw-map "\M-:" 'eval-expression)
  
  (setq term-prompt-regexp "^`--> ")
  
  ;; Redefine the 8 primary terminal colors to look good against black
  (setq ansi-term-color-vector
        [unspecified "#002b36" "#dc322f" "#859900" "#b58900" 
                     "#268bd2" "#d33682" "#2aa198" "#839496"]))

(defun pmade-term-toggle-mode ()
  "Toggle between term-char-mode and term-line-mode."
  (interactive)
  (if (term-in-line-mode) 
      (progn 
        (term-char-mode)
        (term-send-raw-string "\C-e"))
    (term-line-mode)))

(defun pmade-term-yank ()
  "Allow yank to work in raw char mode"
  (interactive)
  (term-send-raw-string (current-kill 0))) 

(add-hook 'term-mode-hook 'pmade-term-mode-hook)

;; Settings for all comint based modes
(defun pmade-comint-mode-hook()
  (setq comint-process-echoes t)
  (ansi-color-for-comint-mode-on))

(add-hook 'comint-mode-hook 'pmade-comint-mode-hook)
