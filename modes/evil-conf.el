;;; evil-conf.el -- Settings for Evil.
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'spaceline) ; Load faces.

;; Settings:
(custom-set-variables
 '(evil-shift-width 2)
 '(evil-default-cursor 'box)
 '(evil-move-beyond-eol t)
 '(evil-want-fine-undo t)
 '(evil-want-Y-yank-to-eol t)
 '(evil-lookup-func #'man)
 '(evil-symbol-word-search t))

(defun pjones:evil-update-cursor ()
  "Change the cursor to match the evil state."
  (let* ((cursor
          (cond
           ((evil-normal-state-p)  '(box  . spaceline-evil-normal))
           ((evil-insert-state-p)  '(hbar . spaceline-evil-insert))
           ((evil-emacs-state-p)   '(bar  . spaceline-evil-emacs))
           ((evil-replace-state-p) '(box  . spaceline-evil-replace))
           ((evil-visual-state-p)  '(box  . spaceline-evil-visual))
           ((evil-motion-state-p)  '(box  . spaceline-evil-motion))
           (t                      '(box  . error))))
         (fg (face-attribute (cdr cursor) :foreground))
         (bg (face-attribute (cdr cursor) :background)))
    (setq cursor-type (car cursor))
    (set-face-attribute 'cursor nil :foreground fg :background bg)))

;; Additional key bindings:
(define-key evil-normal-state-map "g " #'just-one-space)
(define-key evil-normal-state-map "gw" #'evil-window-mru)
(define-key evil-normal-state-map "gb" #'evil-switch-to-windows-last-buffer)

;; Hooks:
(add-hook 'post-command-hook #'pjones:evil-update-cursor)
(add-hook 'evil-mode-hook 'evil-commentary-mode)
(add-hook 'evil-mode-hook 'global-evil-fringe-mark-mode)
(add-hook 'evil-mode-hook 'global-evil-surround-mode)
(add-hook 'evil-mode-hook 'global-evil-quickscope-mode)
(add-hook 'evil-mode-hook 'evil-collection-init)

;;; evil-conf.el ends here
