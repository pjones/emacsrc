;; Functions for keybindings
(defun open-line-below-like-vim ()
  "Open a line below the point, and move there"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun open-line-above-like-vim ()
  "Open a line above the point, and move there"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun new-window-with-terminal ()
  "Open a new window, small in size, and run a terminal"
  (interactive)
  (let ((window (split-window-vertically)))
    (select-window window)
    (term (getenv "SHELL"))))

(defun switch-to-previous-buffer ()
  "Switch back to the previous buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun kill-region-or-backward-kill-word (arg)
  "If there is a region, kill it.  Otherwise kill the word before point"
  (interactive "*p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun save-to-kill-ring-and-normalize-whitespace ()
  (interactive)
  (let ((text (buffer-substring (region-beginning) (region-end))))
    (kill-new
     (replace-regexp-in-string "^\s+" "" (replace-regexp-in-string "\n\s*" " " text))))
  (deactivate-mark))

(defun pmade-outline-mode-hook ()
  "Some key bindings and changes for outline mode"
  (local-set-key "\C-j" 'outline-insert-heading))

;; Based on smart tab: http://www.emacswiki.org/cgi-bin/emacs-en/TabCompletion
(defvar pmade-inside-smart-tab 0)

(defun pmade-smart-org-cycle ()
  "Prevent recursion with org-mode"
  (if (= pmade-inside-smart-tab 0)
      (let ((pmade-inside-smart-tab 1)) (org-cycle))
    (indent-for-tab-command)))

(defun pmade-smart-tab ()
  "Context based tab key.  If there is a region, indent the
  region.  Otherwise attempt to perform tab completion or
  indentation."
  (interactive)
  (cond
   ((minibufferp) (minibuffer-complete))
   ((string= major-mode "org-mode") (pmade-smart-org-cycle))
   (mark-active (indent-region (region-beginning) (region-end)))
   ((looking-at "\\_>") (hippie-expand nil))
   (t (indent-for-tab-command))))

;; For iDo
(defun pmade-ido-mode-hook ()
  "Some key bindings and changes for ido mode"
  (define-key ido-file-dir-completion-map "\C-n" 'ido-next-work-directory)
  (define-key ido-file-dir-completion-map "\C-p" 'ido-prev-work-directory)
  (define-key ido-file-completion-map     "\C-w" 'ido-delete-backward-word-updir))

;; Help start ERC
(defun pmade-erc-start (&optional bitlbee-only)
  "Load and start ERC.  With prefix key, only connect to bitlbee."
  (interactive "P")
  (load "~/.emacs.d/pmade/pmade-erc")
  (erc :server "127.0.0.1")
  (erc-tls :server "irc.pmade.com" :port 6697 :password pmade-irc-password)
  (unless bitlbee-only (erc :server "irc.freenode.net")))

;; Better buffer menu
(defun pmade-buffer-menu (&optional arg)
  "Load a buffer-menu and when it quits, restore the window configuration"
  (interactive "P")
  (let ((window-conf (current-window-configuration)))
    (buffer-menu-other-window arg)
    (set (make-local-variable 'pmade-saved-window-configuration) window-conf)
    (define-key Buffer-menu-mode-map "q" 'pmade-quit-buffer-menu)))

(defun pmade-quit-buffer-menu ()
  "Restore the window configuration, closing a buffer window"
  (interactive)
  (let ((window-conf pmade-saved-window-configuration))
    (when window-conf (set-window-configuration window-conf))))

;; For Carbon Emacs only
(setq mac-option-modifier 'meta)

;; Global Key Bindings
(define-key global-map "\C-x\C-m" 'execute-extended-command)          ; Use M-x with the Meta key
(define-key global-map "\C-x\C-b" 'pmade-buffer-menu)                 ; My buffer menu helper
(define-key global-map "\C-o" 'open-line-below-like-vim)              ; Open a newline below and go there
(define-key global-map "\M-o" 'open-line-above-like-vim)              ; Open a newline above and go there
(define-key global-map [(control shift k)] 'kill-whole-line)          ; Shortcut for C-S-backspace
(define-key global-map [?\C-x ?t] 'new-window-with-terminal)          ; Open a new window with a terminal
(define-key global-map [?\C-x ?x] 'switch-to-previous-buffer)         ; Jump to the last buffer
(define-key global-map "\C-w" 'kill-region-or-backward-kill-word)     ; Act more like a shell
(define-key global-map "\t" 'pmade-smart-tab)                         ; Make tab smarter
(define-key global-map [?\C-c ?\M-w] 'save-to-kill-ring-and-normalize-whitespace)

;; These are mostly for terminal emacs, since C-0 through C-9 don't
;; work there.  I use M-0 through M-9 with the window-number package,
;; but still need a way to provide digit arguments to functions.
(define-key global-map "\C-x\M-0" 'digit-argument)
(define-key global-map "\C-x\M-1" 'digit-argument)
(define-key global-map "\C-x\M-2" 'digit-argument)
(define-key global-map "\C-x\M-3" 'digit-argument)
(define-key global-map "\C-x\M-4" 'digit-argument)
(define-key global-map "\C-x\M-5" 'digit-argument)
(define-key global-map "\C-x\M-6" 'digit-argument)
(define-key global-map "\C-x\M-7" 'digit-argument)
(define-key global-map "\C-x\M-8" 'digit-argument)
(define-key global-map "\C-x\M-9" 'digit-argument)

;; Function Keys
(define-key global-map [f2] 'org-go-to-remember-target)

;; Key Bindings for Working with Windows
(define-key global-map [(meta down)]     'shrink-window)              ; Make window smaller (vertical)
(define-key global-map [(meta up)]       'enlarge-window)             ; Make window bigger (vertical)
(define-key global-map [(meta left)]     'shrink-window-horizontally) ; Make window smaller
(define-key global-map [(meta right)]    'enlarge-window-horizontally); Make window bigger

;; I freaking hate C-z (unless I'm using ElScreen)
(unless (fboundp 'elscreen-create) (define-key global-map "\C-z" nil))

;; If I am using ElScreen, however...
(when (fboundp 'elscreen-create)
  (define-key global-map "\C-z\C-z" 'elscreen-toggle))

;; Outline and Org Mode Bindings
(add-hook 'outline-mode-hook 'pmade-outline-mode-hook)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cr" 'org-remember)

;; ido key bindings
(add-hook 'ido-setup-hook 'pmade-ido-mode-hook)