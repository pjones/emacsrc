;;; markdown-conf.el -- Settings for markdown-mode.
;;
;;; Commentary:
;;
;;; Code:
(require 'darkroom)
(require 'markdown-mode)
(require 'visual-fill)
(require 'whitespace)
(require 'yasnippet)

(declare-function pjones:open-line-above "../lisp/interactive.el")
(declare-function pjones:indent-or-complete "../lisp/completion.el")

(autoload 'org-open-file "org")

;; Basic settings.
(custom-set-variables
 '(markdown-header-scaling nil)
 '(markdown-reference-location 'end)
 '(markdown-asymmetric-header t)
 '(markdown-hide-urls t)
 '(markdown-command
   (concat "pandoc -s --mathjax --filter "
           (pjones:script "pandoc-filter-title.sh"))))

(custom-set-faces
 '(markdown-code-face ((t (:background nil))))
 '(markdown-header-delimiter-face ((t (:inherit org-done))))
 '(markdown-header-face-1 ((t (:inherit outline-1 :height 2.0))))
 '(markdown-header-face-2 ((t (:inherit outline-2 :height 1.7))))
 '(markdown-header-face-3 ((t (:inherit outline-3 :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit outline-4 :height 1.1))))
 '(markdown-header-face-5 ((t (:inherit outline-5))))
 '(markdown-header-face-6 ((t (:inherit outline-6)))))

(defvar pjones:markdown-attachments-directory
  "attachments"
  "Name of the directory used to hold git-annex files.")

(defun pjones:markdown-insert-list-item (&optional arg)
  "Insert a new list item.

Unlike the markdown version of this function, leave a blank line
between the old and new list items.

With a \\[universal-argument] prefix (i.e., when ARG is (4)),
decrease the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
increase the indentation by one level."
  (interactive "p")
  (markdown-insert-list-item (or arg 0))
  (save-excursion
    (pjones:open-line-above nil)))

(defun pjones:markdown-insert-heading-or-item (reverse)
  "Insert a heading or item based on the current context.
If REVERSE is non-nil, do the opposite of what the context says."
  (interactive "P")
  (if (or (markdown-on-heading-p) reverse)
      (progn
        (newline)
        (markdown-insert-header-dwim))
    (pjones:markdown-insert-list-item)))

(defun pjones:markdown-visual-line ()
  "Don't wrap lines.  Needed for most web forms."
  (interactive)
  (auto-fill-mode -1)
  (whitespace-mode -1)
  (visual-line-mode)
  (visual-fill-mode)
  (darkroom-mode))

(defun pjones:markdown-follow-thing-at-point (arg)
  "Call (and pass ARG) to `markdown-follow-thing-at-point'."
  (interactive "P")
  (require 'org)
  (cl-letf (((symbol-function 'find-file)
             (lambda (name) (org-open-file name))))
    (markdown-follow-thing-at-point arg)))

(defun pjones:markdown-bind-keys ()
  "Bind keys in modes derived from `markdown-mode'."
  (let* ((mode major-mode)
         (map (symbol-value (intern (concat (symbol-name mode) "-map")))))
    (define-key map (kbd "C-<return>") #'pjones:markdown-insert-heading-or-item)
    (define-key map (kbd "TAB") #'pjones:indent-or-complete)
    (define-key map (kbd "C-c C-c") #'markdown-preview)))

(defun pjones:markdown-mode-hook ()
  "Set up key bindings and other crap for markdown-mode."
  (whitespace-mode)
  (yas-minor-mode)

  ;; Translate some strings into pretty symbols:
  (setq prettify-symbols-alist
        '(("[x]" . (?\] (Bl . Br) ?✓ (Bl . Br) ?\[)))))

(add-hook 'markdown-mode-hook 'pjones:markdown-mode-hook)
(add-hook 'markdown-mode-hook 'pjones:markdown-bind-keys)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; markdown-mode-conf.el ends here
