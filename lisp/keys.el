;;; keys.el -- Global key bindings.
;;; Commentary:
;;; Code:
;; ;; Emacs nonstandard editing commands
;; (autoload 'zap-up-to-char "misc" nil t)
;; (autoload 'pjones:browse-url (concat pjones:modes-dir "browse-url-conf") nil t)
;; (autoload 'pjones:browse-url-shortcut (concat pjones:modes-dir "browse-url-conf") nil t)

;; (defvar pjones:z-map (make-sparse-keymap)
;;   "A map of key bindings under z.")

;; Key bindings relating to god-mode.
;; (require 'god-mode)
;; (global-set-key (kbd "<escape>") 'god-mode-all)
;; (define-key god-local-mode-map (kbd "z") pjones:z-map)

;; Overriding default key bindings
(global-set-key (kbd "C-x C-c")   'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-b")   'ivy-switch-buffer)
(global-set-key (kbd "C-x b")     'ivy-switch-buffer)
(global-set-key (kbd "C-x M-b")   'ibuffer)
(global-set-key (kbd "C-x C-f")   'find-file)
(global-set-key (kbd "C-x C-o")   'switch-window)
(global-set-key (kbd "C-x o")     'switch-window)
(global-set-key (kbd "C-x M-o")   'resize-window)
(global-set-key (kbd "C-x C-x")   'pjones:exchange-point-and-mark)
(global-set-key (kbd "C-h M-m")   'describe-mode)
(global-set-key (kbd "C-a")       'pjones:move-beginning-of-line)
(global-set-key (kbd "C-o")       'pjones:open-line-above)
(global-set-key (kbd "M-o")       (lambda () (interactive) (pjones:open-line-above t)))
(global-set-key (kbd "C-s")       'isearch-forward-regexp)
(global-set-key (kbd "C-r")       'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")     'isearch-forward)
(global-set-key (kbd "C-M-r")     'isearch-backward)
(global-set-key (kbd "C-w")       'pjones:kill-region-or-backward-kill-word)
(global-set-key (kbd "M-g C-c")   'goto-char)
(global-set-key (kbd "M-g C-l")   'goto-line)
(global-set-key (kbd "M-g C-n")   'next-error)
(global-set-key (kbd "M-g C-p")   'previous-error)
(global-set-key (kbd "M-x")       'counsel-M-x)
(global-set-key (kbd "M-y")       'counsel-yank-pop)
(global-set-key (kbd "M-z")       'zap-up-to-char)
(global-set-key (kbd "C-M-z")     'zap-to-char)
(global-set-key (kbd "M-/")       'company-complete)
(global-set-key (kbd "M-'")       'pjones:zap-to-quote)
(global-set-key (kbd "M-<tab>")   'hs-toggle-hiding)
(global-set-key (kbd "C-x 0")     'pjones:switch-window-then-delete)
(global-set-key (kbd "C-x C-0")   'pjones:switch-window-then-delete)
(global-set-key (kbd "C-x C-1")   'delete-other-windows)
(global-set-key (kbd "C-x C-2")   'split-window-below)
(global-set-key (kbd "C-x C-3")   'split-window-right)
(global-set-key (kbd "<home>")    'beginning-of-buffer)
(global-set-key (kbd "<end>")     'end-of-buffer)

;; User Key Bindings (using the C-z prefix)
;; (global-set-key (kbd "C-z") pjones:z-map)
;;
;; (define-key pjones:z-map (kbd "RET") 'pjones:start-term)
;; (define-key pjones:z-map (kbd ";")   'pjones:auto-correct-previous-word)
;; (define-key pjones:z-map (kbd ",")   'pjones:push-tag-mark)
;; (define-key pjones:z-map (kbd ".")   'quick-calc)
;; (define-key pjones:z-map (kbd "a")   'align)
;; (define-key pjones:z-map (kbd "A")   'ialign)
;; (define-key pjones:z-map (kbd "b")   'pjones:bm-bookmark-set)
;; (define-key pjones:z-map (kbd "B")   'pjones:bookmark)
;; (define-key pjones:z-map (kbd "c")   'pjones:projectile-compile-project)
;; (define-key pjones:z-map (kbd "d")   'pjones:projectile-dired)
;; (define-key pjones:z-map (kbd "e")   'hydra-launch/body)
;; (define-key pjones:z-map (kbd "f")   'pjones:kill-file-name)
;; (define-key pjones:z-map (kbd "g")   'rgrep)
;; (define-key pjones:z-map (kbd "h")   'hl-line-mode)
;; (define-key pjones:z-map (kbd "H")   'highlight-regexp)
;; (define-key pjones:z-map (kbd "i")   'counsel-imenu)
;; (define-key pjones:z-map (kbd "I")   'pjones:uuid)
;; (define-key pjones:z-map (kbd "j")   'pjones:bm-bookmark-jump)
;; (define-key pjones:z-map (kbd "J")   'pjones:journal)
;; (define-key pjones:z-map (kbd "m")   'magit-status)
;; (define-key pjones:z-map (kbd "n")   'pjones:insert-italian-name)
;; (define-key pjones:z-map (kbd "o")   'pjones:browse-url)
;; (define-key pjones:z-map (kbd "O")   'pjones:browse-url-shortcut)
;; (define-key pjones:z-map (kbd "p")   'passmm-completing-read)
;; (define-key pjones:z-map (kbd "P")   'pjones:pigeon)
;; (define-key pjones:z-map (kbd "r")   'pjones:register-get-set)
;; (define-key pjones:z-map (kbd "R")   'revert-buffer)
;; (define-key pjones:z-map (kbd "s")   'sort-lines)
;; (define-key pjones:z-map (kbd "t")   'pjones:start-term)
;; (define-key pjones:z-map (kbd "u")   'goto-last-change)
;; (define-key pjones:z-map (kbd "w")   'hydra-window-ops/body)
;; (define-key pjones:z-map (kbd "z")   'exwm-nw-goto-previou<s)
;; (define-key pjones:z-map (kbd "C-z") 'exwm-nw-goto-previous)



(defvar pjones:leader-map-e (make-sparse-keymap)
  "A map of key bindings under e.")

(setq evil-leader/leader ","              ; Duh.
      evil-collection-setup-minibuffer t) ; Consistency.

(require 'evil)
(require 'evil-leader)

(evil-leader/set-key
  "A" 'align
  "a" 'ialign
  "c" 'pjones:projectile-compile-project
  "d" 'pjones:projectile-dired
  "s" 'sort-lines

  ;; Buffers:
  "b b" 'ivy-switch-buffer
  "b i" 'ibuffer
  "b k" 'kill-buffer
  "b o" 'counsel-find-file
  "b r" 'revert-buffer
  "b s" 'save-buffer

  ;; Execute:
  "e a"   'pjones:agenda
  "e c"   'org-capture
  "e g"   'magit-status
  "e h"   'pjones:start-http
  "e i"   'pjones:start-irc
  "e j c" 'pjones:indium-start-chrome
  "e j n" 'pjones:indium-start-node
  "e l"   'pjones:lock-screen
  "e m"   'pjones:start-mail
  "e s"   'org-store-link
  "e t"   'pjones:start-term

  ;; Passwords:
  "p" nil
  "p p" 'passmm-completing-read
  "p g" 'pjones:pwgen
  "p l" 'passmm-list-passwords

  ;; Windows:
  "w" 'evil-window-map)

;; Turn on Evil!
(global-evil-leader-mode)
(evil-mode)

;;; keys.el ends here
