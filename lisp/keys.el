;;; keys.el -- Global key bindings.
;;; Commentary:
;;; Code:

;; For Macintosh Emacs only
;; (setq mac-option-modifier  'hyper
;;       mac-command-modifier 'meta))

;; Emacs nonstandard editing commands
(autoload 'zap-up-to-char "misc" nil t)

(defvar pjones:z-map (make-sparse-keymap)
  "A map of key bindings under z.")

;; Key bindings relating to god-mode.
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode)
(define-key god-local-mode-map (kbd "z") pjones:z-map)

;; Overriding default key bindings
(global-set-key (kbd "C-x C-c")   'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-b")   'ibuffer)
(global-set-key (kbd "C-x C-f")   'helm-find-files)
(global-set-key (kbd "C-x b")     'helm-mini)
(global-set-key (kbd "C-x o")     'switch-window)
(global-set-key (kbd "C-x O")     'resize-window)
(global-set-key (kbd "C-x M-o")   'pjones:switch-window-then-delete)
(global-set-key (kbd "C-a")       'pjones:move-beginning-of-line)
(global-set-key (kbd "C-o")       'pjones:open-line-above)
(global-set-key (kbd "M-o")       (lambda () (interactive) (pjones:open-line-above t)))
(global-set-key (kbd "C-s")       'isearch-forward-regexp)
(global-set-key (kbd "C-r")       'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")     'isearch-forward)
(global-set-key (kbd "C-M-r")     'isearch-backward)
(global-set-key (kbd "C-w")       'pjones:kill-region-or-backward-kill-word)
(global-set-key (kbd "M-x")       'helm-M-x)
(global-set-key (kbd "M-y")       'helm-show-kill-ring)
(global-set-key (kbd "M-z")       'zap-up-to-char)
(global-set-key (kbd "M-S-z")     'zap-to-char)
(global-set-key (kbd "M-/")       'company-complete)
(global-set-key (kbd "M-'")       'pjones:zap-to-quote)
(global-set-key (kbd "C-+")       'text-scale-increase)
(global-set-key (kbd "C--")       'text-scale-decrease)
(global-set-key (kbd "M-<tab>")   'hs-toggle-hiding)
(global-set-key (kbd "C-x C-1")   'delete-other-windows)
(global-set-key (kbd "C-x C-2")   'split-window-below)
(global-set-key (kbd "C-x C-3")   'split-window-right)
(global-set-key (kbd "C-x C-0")   'delete-window)
(global-set-key (kbd "<home>")    'beginning-of-buffer)
(global-set-key (kbd "<end>")     'end-of-buffer)

;; Remapping existing keys.
(global-set-key [remap goto-line] 'pjones:goto-line-with-feedback)

;; User Key Bindings (using the C-z prefix)
(global-set-key (kbd "C-z") pjones:z-map)

(define-key pjones:z-map (kbd "SPC") 'just-one-space)
(define-key pjones:z-map (kbd ";")   'pjones:auto-correct-previous-word)
(define-key pjones:z-map (kbd ",")   'pjones:push-tag-mark)
(define-key pjones:z-map (kbd ".")   'quick-calc)
(define-key pjones:z-map (kbd "a")   'align)
(define-key pjones:z-map (kbd "A")   'ialign)
(define-key pjones:z-map (kbd "b")   'pjones:bookmark)
(define-key pjones:z-map (kbd "c")   'projectile-compile-project)
(define-key pjones:z-map (kbd "d")   'projectile-dired)
;; (define-key pjones:z-map (kbd "D")   'helm-dictionary)
(define-key pjones:z-map (kbd "e")   'hydra-launch/body)
(define-key pjones:z-map (kbd "f")   'pjones:kill-file-name)
(define-key pjones:z-map (kbd "g")   'rgrep)
(define-key pjones:z-map (kbd "h")   'hl-line-mode)
(define-key pjones:z-map (kbd "H")   'highlight-regexp)
(define-key pjones:z-map (kbd "i")   'helm-semantic-or-imenu)
(define-key pjones:z-map (kbd "I")   'pjones:uuid)
(define-key pjones:z-map (kbd "J")   'pjones:journal)
(define-key pjones:z-map (kbd "l")   'pjones:jump-back-and-forth)
(define-key pjones:z-map (kbd "m")   'magit-status)
(define-key pjones:z-map (kbd "n")   'pjones:insert-italian-name)
(define-key pjones:z-map (kbd "o")   'browse-url)
(define-key pjones:z-map (kbd "p")   'pjones:pwgen)
(define-key pjones:z-map (kbd "r")   'pjones:register-get-set)
(define-key pjones:z-map (kbd "R")   'revert-buffer)
(define-key pjones:z-map (kbd "s")   'sort-lines)
(define-key pjones:z-map (kbd "u")   'goto-last-change)
(define-key pjones:z-map (kbd "w")   'hydra-window-ops/body)
(define-key pjones:z-map (kbd "z")   'pjones:switch-to-previous-buffer)
(define-key pjones:z-map (kbd "C-z") 'pjones:switch-to-previous-buffer)

;; Super key for Exwm and elscreen:
(global-set-key (kbd "s-x")   'helm-run-external-command)
(global-set-key (kbd "s-SPC") 'helm-elscreen)

(provide 'keys)
;;; keys.el ends here
