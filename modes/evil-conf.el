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
(require 'evil-textobj-syntax)

;; Settings:
(custom-set-variables
 '(evil-want-abbrev-expand-on-insert-exit nil)
 '(evil-shift-width 2)
 '(evil-move-beyond-eol t)
 '(evil-cross-lines t)
 '(evil-lookup-func #'dictionary-lookup-definition)
 '(evil-symbol-word-search t)
 '(evil-search-wrap nil)
 '(evil-search-module 'evil-search)
 '(evil-magic 'very-magic)
 '(evil-ex-search-vim-style-regexp t)
 '(evil-ex-search-persistent-highlight nil)
 '(evil-ex-substitute-case 'smart)
 '(evil-fringe-mark-show-special t)
 '(evil-fringe-mark-ignore-chars '(?' ?{ ?} ?^ ?.))
 '(evil-owl-display-method 'window)
 '(evil-owl-max-string-length 60)
 '(evil-default-cursor #'pjones:evil-set-cursor)
 '(evil-normal-state-cursor 'box)
 '(evil-visual-state-cursor 'hbar)
 '(evil-motion-state-cursor 'hbar)
 '(evil-operator-state-cursor 'hbar)
 '(evil-replace-state-cursor 'box)
 '(evil-emacs-state-cursor 'bar)
 '(evil-insert-state-cursor 'bar)
 '(evil-emacs-state-modes nil)
 '(evil-motion-state-modes nil))

;; Baffles me why I need to do this here, instead of in
;; `custom-set-variables':
(customize-set-variable 'evil-want-Y-yank-to-eol t)

(custom-set-faces
 '(evil-fringe-mark-file-face ((t (:inherit fringe))))
 '(evil-fringe-mark-local-face ((t (:inherit fringe))))
 '(evil-fringe-mark-special-face ((t (:inherit fringe)))))

(defun pjones:evil-search-word (oldfun &rest args)
  "Work around a bug.
https://github.com/emacs-evil/evil/issues/347
OLDFUN is the original `evil-ex-start-word-search' function and ARGS
are the arguments to it."
  (let ((evil-ex-search-vim-style-regexp nil))
    (apply oldfun args)))
(advice-add #'evil-ex-start-word-search :around #'pjones:evil-search-word)

(defun pjones:evil-set-cursor ()
  "Called from `evil-mode' to set the cursor."
  (let ((face (intern (format "doom-modeline-evil-%s-state" evil-state))))
    (when (facep face)
      (evil-set-cursor-color
      (face-attribute face :foreground nil t)))))

;; Remove search highlighting after using C-l:
(advice-add #'recenter-top-bottom :after
            '(lambda (&rest _args) (evil-ex-nohighlight)))

(defun pjones:evil-leader-in-all-states ()
  "Use both leader keys from insert mode."
  ;; Force a local mode keymap.
  (evil-leader/set-key-for-mode major-mode nil nil)
  (let* ((mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
         (map (or mode-map evil-leader--default-map)))
    (define-key evil-insert-state-local-map (kbd "C-SPC") map)
    (define-key evil-insert-state-local-map (kbd "C-<backspace>") map)))

(defun pjones:evil-range-for-comment-paragraph (&optional inclusive)
  "Create a range that includes the currently commented paragraph.
If INCLUSIVE is non-nil, also include leading and trailing white space."
  (let* ((commented-blank-line-p
          (lambda (n)
            (save-excursion
              (forward-line n)
              (back-to-indentation)
              (or
               ;; Line starts with a comment character.
               (looking-at-p
                (rx (and
                     (regexp comment-start-skip)
                     (* (char blank))
                     line-end)))
               ;; Multi-line comment.
               (and
                (nth 4 (syntax-ppss))
                (looking-at-p
                 (rx (and
                      (* (char blank))
                      line-end))))))))
         (commented-line-p
          (lambda (n)
            "Return non-nil if the current line is a non-blank comment."
            (save-excursion
              (forward-line n)
              (back-to-indentation)
              (and
               (not (funcall commented-blank-line-p 0))
               (or
                (looking-at-p (rx (regexp comment-start-skip)))
                (nth 4 (syntax-ppss)))))))
         (comment-boundry
          (lambda (fun n)
            (save-excursion
              (while (funcall fun n)
                (forward-line n))
              (if (> n 0)
                  (end-of-line)
                (back-to-indentation))
              (point))))
         (beg (funcall comment-boundry commented-line-p -1))
         (end (funcall comment-boundry commented-line-p 1)))
    (if inclusive
        (save-excursion
          (evil-range
           (progn
             (goto-char beg)
             (funcall comment-boundry commented-blank-line-p -1))
           (progn
             (goto-char end)
             (+ 1 (funcall comment-boundry commented-blank-line-p 1)))))
      (evil-range beg end))))

(evil-define-text-object pjones:evil-i-comment-para
  (_count &optional _beg _end _type)
  "Select inner comment paragraph."
  (pjones:evil-range-for-comment-paragraph))

(evil-define-text-object pjones:evil-a-comment-para
  (_count &optional _beg _end _type)
  "Select outer comment paragraph."
  (pjones:evil-range-for-comment-paragraph t))

(define-key evil-inner-text-objects-map "c" #'pjones:evil-i-comment-para)
(define-key evil-outer-text-objects-map "c" #'pjones:evil-a-comment-para)

;; Hooks:
(add-hook 'evil-leader-mode-hook #'pjones:evil-leader-in-all-states)
(add-hook 'evil-mode-hook #'evil-commentary-mode)
(add-hook 'evil-mode-hook #'evil-owl-mode)
(add-hook 'evil-mode-hook #'global-evil-fringe-mark-mode)
(add-hook 'evil-mode-hook #'global-evil-matchit-mode)
(add-hook 'evil-mode-hook #'global-evil-surround-mode)
(add-hook 'pjones:after-theme-change-hook #'pjones:evil-set-cursor)

;;; evil-conf.el ends here
