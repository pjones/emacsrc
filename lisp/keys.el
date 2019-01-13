;;; keys.el -- Global key bindings.
;;; Commentary:
;;; Code:

;; Overriding default key bindings
(global-set-key (kbd "C-h M-m")   #'describe-mode)
(global-set-key (kbd "C-r")       #'isearch-backward-regexp)
(global-set-key (kbd "C-s")       #'isearch-forward-regexp)
(global-set-key (kbd "C-w")       #'pjones:kill-region-or-backward-kill-word)
(global-set-key (kbd "C-x 0")     #'pjones:switch-window-then-delete)
(global-set-key (kbd "C-x C-c")   #'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-f")   #'counsel-find-file)
(global-set-key (kbd "C-x M-o")   #'resize-window)
(global-set-key (kbd "C-x b")     #'switch-to-buffer)
(global-set-key (kbd "C-x o")     #'switch-window)
(global-set-key (kbd "M-/")       #'company-complete)
(global-set-key (kbd "M-x")       #'counsel-M-x)
(global-set-key (kbd "M-y")       #'counsel-yank-pop)

;; Custom bindings under C-c:
(defvar pjones:b-map (make-sparse-keymap)
  "A map of buffer focused key bindings.")

(defvar pjones:e-map (make-sparse-keymap)
  "A map of keys for executing things.")

(defvar pjones:p-map (make-sparse-keymap)
  "A map of password related keys.")

(let ((map pjones:b-map))
  (define-key map (kbd "b") #'switch-to-buffer)
  (define-key map (kbd "f") #'find-file)
  (define-key map (kbd "i") #'ibuffer)
  (define-key map (kbd "k") #'kill-buffer)
  (define-key map (kbd "n") #'pjones:kill-file-name)
  (define-key map (kbd "r") #'revert-buffer))

(let ((map pjones:e-map))
  (define-key map (kbd "a")   #'pjones:agenda)
  (define-key map (kbd "c")   #'org-capture)
  (define-key map (kbd "d")   #'treemacs)
  (define-key map (kbd "h")   #'pjones:start-http)
  (define-key map (kbd "i")   #'pjones:start-irc)
  (define-key map (kbd "j c") #'pjones:indium-start-chrome)
  (define-key map (kbd "j n") #'pjones:indium-start-node)
  (define-key map (kbd "l")   #'pjones:lock-screen)
  (define-key map (kbd "m")   #'pjones:start-mail)
  (define-key map (kbd "q")   #'quick-calc)
  (define-key map (kbd "r")   #'rgrep)
  (define-key map (kbd "s")   #'org-store-link)
  (define-key map (kbd "t")   #'pjones:start-term))

(let ((map pjones:p-map))
  (define-key map (kbd "g") #'pjones:pwgen)
  (define-key map (kbd "p") #'passmm-completing-read)
  (define-key map (kbd "l") #'passmm-list-passwords))

;; User bindings under C-c:
(global-set-key (kbd "C-c b")   pjones:b-map)
(global-set-key (kbd "C-c c") #'pjones:projectile-compile-project)
(global-set-key (kbd "C-c d") #'pjones:projectile-dired)
(global-set-key (kbd "C-c e")   pjones:e-map)
(global-set-key (kbd "C-c f") #'pjones:projectile-find-file)
(global-set-key (kbd "C-c g") #'projectile-grep)
(global-set-key (kbd "C-c h") #'hl-line-mode)
(global-set-key (kbd "C-c H") #'highlight-regexp)
(global-set-key (kbd "C-c i") #'counsel-imenu)
(global-set-key (kbd "C-c m") #'magit-status)
(global-set-key (kbd "C-c p")   pjones:p-map)
(global-set-key (kbd "C-c t") #'org-mru-clock-in)

;; Evil and evil-leader:
;; These need to be set before loading Evil.
(setq evil-leader/leader ","              ; Duh.
      evil-collection-company-use-tng nil ; Turn that crap off!
      evil-collection-term-sync-state-and-mode-p nil
      evil-collection-setup-minibuffer t) ; Consistency.

(require 'evil)
(require 'evil-leader)

(evil-leader/set-key
  "A" #'align
  "a" #'ialign
  "s" #'sort-lines)

;; Turn on Evil!
(global-evil-leader-mode)
(evil-mode)

;;; keys.el ends here
