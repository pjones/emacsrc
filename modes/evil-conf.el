;;; evil-conf.el -- Settings for Evil.
;;
;;; Commentary:
;;
;;; Code:
(require 'doom-modeline-core)
(require 'evil)
(require 'evil-matchit)
(require 'evil-owl)
(require 'evil-surround)

;; Settings:
(custom-set-variables
 '(evil-shift-width 2)
 '(evil-move-beyond-eol t)
 '(evil-want-fine-undo nil)
 '(evil-want-Y-yank-to-eol t)
 '(evil-cross-lines t)
 '(evil-lookup-func #'man)
 '(evil-symbol-word-search t)
 '(evil-search-module 'evil-search)
 '(evil-fringe-mark-show-special t)
 '(evil-fringe-mark-ignore-chars '(?' ?{ ?} ?^ ?.))
 '(evil-owl-display-method 'window)
 '(evil-owl-max-string-length 50))

(custom-set-faces
 '(evil-fringe-mark-local-face ((t (:inherit fringe))))
 '(evil-fringe-mark-file-face ((t (:inherit fringe))))
 '(evil-fringe-mark-special-face ((t (:inherit fringe)))))

(defun pjones:evil-mode-hook ()
  "Hook fun by `evil-mode'."
  ;; Use symbols for movement instead of words!
  (defalias 'forward-evil-word 'forward-evil-symbol)
  ;; Put buffers back into the correct mode after saving them:
  (add-hook 'after-save-hook #'evil-change-to-initial-state))

(defun pjones:evil-set-cursors ()
  "Settings that need to be applied after init."
  (let ((normal   (face-attribute 'doom-modeline-evil-normal-state   :foreground nil t))
        (visual   (face-attribute 'doom-modeline-evil-visual-state   :foreground nil t))
        (motion   (face-attribute 'doom-modeline-evil-motion-state   :foreground nil t))
        (operator (face-attribute 'doom-modeline-evil-operator-state :foreground nil t))
        (replace  (face-attribute 'doom-modeline-evil-replace-state  :foreground nil t))
        (emacs    (face-attribute 'doom-modeline-evil-emacs-state    :foreground nil t))
        (insert   (face-attribute 'doom-modeline-evil-insert-state   :foreground nil t)))
    (setq evil-default-cursor        (list 'box normal)
          evil-normal-state-cursor   (list 'box normal)
          evil-visual-state-cursor   (list 'hbar visual)
          evil-motion-state-cursor   (list 'hbar motion)
          evil-operator-state-cursor (list 'hbar operator)
          evil-replace-state-cursor  (list 'box replace)
          evil-emacs-state-cursor    (list 'bar emacs)
          evil-insert-state-cursor   (list 'bar insert))))

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
(add-hook 'after-init-hook   #'pjones:evil-set-cursors)
(add-hook 'evil-mode-hook    #'evil-collection-init)
(add-hook 'evil-mode-hook    #'evil-commentary-mode)
(add-hook 'evil-mode-hook    #'evil-owl-mode)
(add-hook 'evil-mode-hook    #'global-evil-fringe-mark-mode)
(add-hook 'evil-mode-hook    #'global-evil-matchit-mode)
(add-hook 'evil-mode-hook    #'global-evil-surround-mode)
(add-hook 'evil-mode-hook    #'pjones:evil-mode-hook)

;;; evil-conf.el ends here
