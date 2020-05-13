;;; keys.el -- Global key bindings.
;;
;;; Commentary:
;;
;;; Code:
(declare-function pjones:agenda "./interactive.el")
(declare-function pjones:evil-sort "../modes/evil-conf.el")
(declare-function pjones:indent-or-complete "./completion.el")
(declare-function pjones:indium-start-chrome "./interactive.el")
(declare-function pjones:indium-start-node "./interactive.el")
(declare-function pjones:kill-file-name "./interactive.el")
(declare-function pjones:kill-region-or-backward-kill-word "./interactive.el")
(declare-function pjones:lock-screen "./interactive.el")
(declare-function pjones:maybe-save-buffers-kill-terminal "./interactive.el")
(declare-function pjones:projectile-compile-project "./interactive.el")
(declare-function pjones:projectile-dired "./interactive.el")
(declare-function pjones:projectile-find-file "./interactive.el")
(declare-function pjones:pwgen "./interactive.el")
(declare-function pjones:start-http "./interactive.el")
(declare-function pjones:start-irc "./interactive.el")
(declare-function pjones:start-mail "./interactive.el")
(declare-function pjones:start-term "./interactive.el")
(declare-function pjones:switch-window-then-delete "./interactive.el")

;; Overriding default key bindings
(global-set-key (kbd "C-h M-m")   #'describe-mode)
(global-set-key (kbd "C-r")       #'isearch-backward-regexp)
(global-set-key (kbd "C-s")       #'isearch-forward-regexp)
(global-set-key (kbd "C-w")       #'pjones:kill-region-or-backward-kill-word)
(global-set-key (kbd "C-x C-c")   #'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-f")   #'counsel-find-file)
(global-set-key (kbd "C-x M-o")   #'resize-window)
(global-set-key (kbd "C-x b")     #'switch-to-buffer)
(global-set-key (kbd "C-x o")     #'switch-window)
(global-set-key (kbd "C-+")       #'text-scale-adjust)
(global-set-key (kbd "C--")       #'text-scale-adjust)
(global-set-key (kbd "M-/")       #'company-complete)
(global-set-key (kbd "M-x")       #'counsel-M-x)
(global-set-key (kbd "M-y")       #'counsel-yank-pop)
(global-set-key (kbd "TAB")       #'pjones:indent-or-complete)

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
  (define-key map (kbd "h")   #'pjones:start-http)
  (define-key map (kbd "i")   #'pjones:start-irc)
  (define-key map (kbd "j c") #'pjones:indium-start-chrome)
  (define-key map (kbd "j n") #'pjones:indium-start-node)
  (define-key map (kbd "m")   #'pjones:start-mail)
  (define-key map (kbd "q")   #'quick-calc)
  (define-key map (kbd "r")   #'rgrep)
  (define-key map (kbd "s")   #'org-store-link)
  (define-key map (kbd "t")   #'treemacs)
  (define-key map (kbd "x")   #'pjones:search-backwards-browse-url))

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
(global-set-key (kbd "C-c g") #'deadgrep)
(global-set-key (kbd "C-c h") #'hl-line-mode)
(global-set-key (kbd "C-c H") #'highlight-regexp)
(global-set-key (kbd "C-c i") #'counsel-imenu)
(global-set-key (kbd "C-c m") #'magit-status)
(global-set-key (kbd "C-c p")   pjones:p-map)
(global-set-key (kbd "C-c t") #'org-mru-clock-in)

;; Evil and evil-leader:
;; These need to be set before loading Evil.
(custom-set-variables
 '(evil-leader/leader "SPC")
 '(evil-collection-company-use-tng nil) ; Turn that crap off!
 '(evil-collection-term-sync-state-and-mode-p nil))

(require 'evil)
(require 'evil-leader)

(evil-leader/set-key
  "SPC"   #'pjones:switch-to-previous-buffer
  "<tab>" #'pjones:comment-bar
  "a"     #'pjones:evil-align
  "A"     #'ialign
  "b"     #'ivy-switch-buffer
  "c"     #'pjones:projectile-compile-project
  "d"     #'treemacs-select-window
  "e"     pjones:e-map
  "f"     #'counsel-find-file
  "g"     #'next-error
  "j"     #'erc-track-switch-buffer
  "m"     #'magit-status
  "n"     #'pjones:fly-next-error
  "p f"   #'pjones:projectile-find-file
  "q"     #'kill-this-buffer
  "s"     #'pjones:evil-sort
  "w"     #'save-buffer
  "x"     #'counsel-M-x)

;; Turn on Evil!
(global-evil-leader-mode)
(evil-mode)

;;; keys.el ends here
