;;; evil-conf.el -- Settings for Evil.
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'evil-matchit)
(require 'evil-nl-break-undo)
(require 'evil-owl)
(require 'evil-surround)

;; Settings:
(custom-set-variables
 '(evil-shift-width 2)
 '(evil-default-cursor 'box)
 '(evil-move-beyond-eol t)
 '(evil-want-fine-undo nil)
 '(evil-want-Y-yank-to-eol t)
 '(evil-cross-lines t)
 '(evil-lookup-func #'man)
 '(evil-symbol-word-search t)
 '(evil-search-module 'evil-search)
 '(evil-fringe-mark-show-special t)
 '(evil-fringe-mark-ignore-chars '(?' ?{ ?} ?^ ?.))

 '(evil-owl-display-method 'posframe)
 '(evil-owl-extra-posframe-args '(:width 50 :height 20))
 '(evil-owl-max-string-length 50))

(custom-set-faces
 '(evil-fringe-mark-local-face ((t (:inherit fringe))))
 '(evil-fringe-mark-file-face ((t (:inherit fringe))))
 '(evil-fringe-mark-special-face ((t (:inherit fringe)))))

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

(defun pjones:evil-mode-hook ()
  "Hook fun by `evil-mode'."
  ;; Put buffers back into the correct mode after saving them:
  (add-hook 'after-save-hook #'evil-change-to-initial-state))

(evil-define-operator pjones:evil-sort (beg end)
  "Sort text."
  :move-point nil
  :type line
  (save-excursion
    (condition-case nil
        (sort-lines nil beg end)
      (error nil))))

;; Additional key bindings:
(evil-define-key 'normal global-map "g "  #'just-one-space)
(evil-define-key 'normal global-map "g'"  #'pjones:switch-to-previous-buffer)
(evil-define-key 'normal global-map "gs"  #'evil-surround-edit)
(evil-define-key 'visual global-map "s"   #'evil-surround-region)
(evil-define-key 'visual global-map "S"   #'evil-Surround-region)
(evil-define-key 'normal global-map "z'"  #'evil-window-mru)

(define-key evil-normal-state-map (kbd "C-SPC") #'evil-scroll-page-down)
(define-key evil-normal-state-map (kbd "C-DEL") #'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "C-SPC") #'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "C-DEL") #'evil-scroll-page-up)

;; Hybrid evil/Emacs bindings:
(evil-define-key 'insert global-map (kbd "C-a") #'beginning-of-line)
(evil-define-key 'insert global-map (kbd "C-e") #'end-of-line)
(evil-define-key 'insert global-map (kbd "C-k") #'kill-line)

;; Hooks:
(add-hook 'post-command-hook #'pjones:evil-update-cursor)
(add-hook 'evil-mode-hook    #'pjones:evil-mode-hook)
(add-hook 'evil-mode-hook    #'evil-collection-init)
(add-hook 'evil-mode-hook    #'evil-commentary-mode)
(add-hook 'evil-mode-hook    #'evil-owl-mode)
(add-hook 'evil-mode-hook    #'global-evil-fringe-mark-mode)
(add-hook 'evil-mode-hook    #'global-evil-matchit-mode)
(add-hook 'evil-mode-hook    #'global-evil-surround-mode)
(add-hook 'prog-mode-hook    #'evil-nl-break-undo-mode)
(add-hook 'text-mode-hook    #'evil-nl-break-undo-mode)

;;; evil-conf.el ends here
