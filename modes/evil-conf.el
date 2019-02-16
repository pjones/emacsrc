;;; evil-conf.el -- Settings for Evil.
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)

;; Settings:
(custom-set-variables
 '(evil-shift-width 2)
 '(evil-default-cursor 'box)
 '(evil-move-beyond-eol t)
 '(evil-want-fine-undo t)
 '(evil-want-Y-yank-to-eol t)
 '(evil-lookup-func #'man)
 '(evil-symbol-word-search t))

(defface pjones:cursor-normal-face
  '((t (:background "DarkGoldenrod2"
        :inherit 'cursor)))
  "Normal state cursor"
  :group 'evil)

(defface pjones:cursor-insert-face
  '((t (:background "chartreuse3"
        :inherit 'cursor)))
  "Insert state cursor"
  :group 'evil)

(defface pjones:cursor-emacs-face
  '((t (:background "SkyBlue2"
        :inherit 'cursor)))
  "Emacs state cursor"
  :group 'evil)

(defface pjones:cursor-replace-face
  '((t (:background "chocolate"
        :inherit 'cursor)))
  "Replace state cursor"
  :group 'evil)

(defface pjones:cursor-visual-face
  '((t (:background "gray"
        :inherit 'cursor)))
  "Visual state cursor"
  :group 'evil)

(defface pjones:cursor-motion-face
  '((t (:background "plum3"
        :inherit 'cursor)))
  "Motion state cursor"
  :group 'evil)

(defun pjones:evil-update-cursor ()
  "Change the cursor to match the evil state."
  (let* ((cursor
          (cond
           ((evil-normal-state-p)  '(box  . pjones:cursor-normal-face))
           ((evil-insert-state-p)  '(hbar . pjones:cursor-insert-face))
           ((evil-emacs-state-p)   '(bar  . pjones:cursor-emacs-face))
           ((evil-replace-state-p) '(box  . pjones:cursor-replace-face))
           ((evil-visual-state-p)  '(box  . pjones:cursor-visual-face))
           ((evil-motion-state-p)  '(box  . pjones:cursor-motion-face))
           (t                      '(box  . error))))
         (fg (face-attribute (cdr cursor) :foreground))
         (bg (face-attribute (cdr cursor) :background)))
    (setq cursor-type (car cursor))
    (set-face-attribute 'cursor nil :foreground fg :background bg)))

;; Additional key bindings:
(evil-define-key 'normal global-map "g " #'just-one-space)
(evil-define-key 'normal global-map "g'" #'pjones:switch-to-previous-buffer)
(evil-define-key 'visual global-map "s"  #'evil-surround-region)
(evil-define-key 'visual global-map "S"  #'evil-Surround-region)
(evil-define-key 'normal global-map "z'" #'evil-window-mru)

;; Hooks:
(add-hook 'post-command-hook #'pjones:evil-update-cursor)
(add-hook 'evil-mode-hook    #'evil-commentary-mode)
(add-hook 'evil-mode-hook    #'global-evil-fringe-mark-mode)
(add-hook 'evil-mode-hook    #'global-evil-surround-mode)
(add-hook 'evil-mode-hook    #'evil-collection-init)

;;; evil-conf.el ends here
