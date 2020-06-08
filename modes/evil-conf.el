;;; evil-conf.el -- Settings for Evil.
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'align)
  (require 'doom-modeline-core)
  (require 'evil-indent-textobject)
  (require 'evil-matchit)
  (require 'evil-owl)
  (require 'evil-surround))

(require 'evil)
(require 'evil-leader)

;; Settings:
(custom-set-variables
 '(evil-shift-width 2)
 '(evil-move-beyond-eol t)
 '(evil-want-fine-undo nil)
 '(evil-cross-lines t)
 '(evil-lookup-func #'dictionary-lookup-definition)
 '(evil-symbol-word-search t)
 '(evil-search-module 'evil-search)
 '(evil-magic 'very-magic)
 '(evil-ex-search-vim-style-regexp t)
 '(evil-ex-search-persistent-highlight nil)
 '(evil-ex-substitute-case 'smart)
 '(evil-fringe-mark-show-special t)
 '(evil-fringe-mark-ignore-chars '(?' ?{ ?} ?^ ?.))
 '(evil-owl-display-method 'window)
 '(evil-owl-max-string-length 60))

;; Baffles me why I need to do this:
(customize-set-variable 'evil-want-Y-yank-to-eol t)

(custom-set-faces
 '(evil-fringe-mark-local-face ((t (:inherit fringe))))
 '(evil-fringe-mark-file-face ((t (:inherit fringe))))
 '(evil-fringe-mark-special-face ((t (:inherit fringe)))))

(defun pjones:evil-mode-hook ()
  "Hook fun by `evil-mode'."
  ;; Use symbols for movement instead of words!
  (defalias 'pjones:forward-evil-WORD 'forward-evil-WORD)
  (defalias 'pjones:forward-evil-word 'forward-evil-word)
  (defalias 'forward-evil-WORD 'forward-evil-word)
  (defalias 'forward-evil-word 'forward-evil-symbol))

(defun pjones:evil-search-word (oldfun &rest args)
  "Work around a bug.
https://github.com/emacs-evil/evil/issues/347
OLDFUN is the original `evil-ex-start-word-search' function and ARGS
are the arguments to it."
  (let ((evil-ex-search-vim-style-regexp nil))
    (apply oldfun args)))
(advice-add #'evil-ex-start-word-search :around #'pjones:evil-search-word)

(defun pjones:evil-set-cursors (&rest _args)
  "Settings that need to be applied after init.
Ignores arguments passed from various hooks that this function is
called from."
  (let ((normal   (face-attribute 'doom-modeline-evil-normal-state   :foreground nil t))
        (visual   (face-attribute 'doom-modeline-evil-visual-state   :foreground nil t))
        (motion   (face-attribute 'doom-modeline-evil-motion-state   :foreground nil t))
        (operator (face-attribute 'doom-modeline-evil-operator-state :foreground nil t))
        (replace  (face-attribute 'doom-modeline-evil-replace-state  :foreground nil t))
        (emacs    (face-attribute 'doom-modeline-evil-emacs-state    :foreground nil t))
        (insert   (face-attribute 'doom-modeline-evil-insert-state   :foreground nil t)))
    (when cursor-type ;; Don't update a disabled cursor.
      (setq evil-default-cursor        (list 'box normal)
            evil-normal-state-cursor   (list 'box normal)
            evil-visual-state-cursor   (list 'hbar visual)
            evil-motion-state-cursor   (list 'hbar motion)
            evil-operator-state-cursor (list 'hbar operator)
            evil-replace-state-cursor  (list 'box replace)
            evil-emacs-state-cursor    (list 'bar emacs)
            evil-insert-state-cursor   (list 'bar insert)))))

;; Remove search highlighting after using C-l:
(advice-add #'recenter-top-bottom :after '(lambda (&rest _args) (evil-ex-nohighlight)))

(evil-define-operator pjones:evil-sort (beg end)
  "Sort text."
  :move-point nil
  :type line
  (save-excursion
    (condition-case nil
        (sort-lines nil beg end)
      (error nil))))

(evil-define-operator pjones:evil-align (beg end)
  "Align text using `align-rules-list'."
  :type line
  (save-excursion
    (condition-case nil
        (align beg end)
      (error nil))))

;; Additional key bindings:
(evil-define-key 'normal global-map (kbd "g <return>") #'delete-blank-lines)
(evil-define-key 'normal global-map "g " #'just-one-space)
(evil-define-key 'normal global-map "g'" #'pjones:switch-to-previous-buffer)
(evil-define-key 'normal global-map "gs" #'evil-surround-edit)
(evil-define-key 'visual global-map "s"  #'evil-surround-region)
(evil-define-key 'visual global-map "S"  #'evil-Surround-region)
(evil-define-key 'normal global-map "z'" #'evil-window-mru)

;; Swap ' and `
(dolist (state '(normal visual motion))
  (evil-define-key state global-map "'" #'evil-goto-mark)
  (evil-define-key state global-map "`" #'evil-goto-mark-line))

;; Hybrid evil/Emacs bindings:
(evil-define-key 'insert global-map (kbd "C-a") #'beginning-of-line)
(evil-define-key 'insert global-map (kbd "C-e") #'end-of-line)
(evil-define-key 'insert global-map (kbd "C-k") #'kill-line)

;; Second leader key:
(dolist (state '(normal visual motion))
  (evil-define-key state global-map (kbd "DEL") evil-leader--default-map))

(defun pjones:evil-leader-in-all-states ()
  "Use both leader keys from insert mode."
  ;; Force a local mode keymap.
  (evil-leader/set-key-for-mode major-mode nil nil)
  (let* ((mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
         (map (or mode-map evil-leader--default-map)))
    (define-key evil-insert-state-local-map (kbd "C-SPC") map)
    (define-key evil-insert-state-local-map (kbd "C-DEL") map)))

;; Hooks:
(add-hook 'after-init-hook #'pjones:evil-set-cursors)
(add-hook 'after-make-frame-functions #'pjones:evil-set-cursors)
(add-hook 'evil-leader-mode-hook #'pjones:evil-leader-in-all-states)
(add-hook 'evil-mode-hook #'evil-commentary-mode)
(add-hook 'evil-mode-hook #'evil-owl-mode)
(add-hook 'evil-mode-hook #'global-evil-fringe-mark-mode)
(add-hook 'evil-mode-hook #'global-evil-matchit-mode)
(add-hook 'evil-mode-hook #'global-evil-surround-mode)
(add-hook 'evil-mode-hook #'pjones:evil-mode-hook)
(add-hook 'evil-mode-hook #'pjones:evil-set-cursors)
(add-hook 'pjones:after-theme-change-hook #'pjones:evil-set-cursors)

;;; evil-conf.el ends here
