;;; markdown-conf.el -- Settings for markdown-mode.
;;
;;; Commentary:
;;
;;; Code:
(require 'company)
(require 'darkroom)
(require 'evil)
(require 'evil-leader)
(require 'markdown-mode)
(require 'visual-fill)
(require 'whitespace)

(declare-function pjones:open-line-above "../lisp/interactive.el")
(declare-function pjones:add-fixme-lock "../lisp/code.el")
(autoload 'org-open-file "org")

;; Basic settings.
(custom-set-variables
 '(markdown-header-scaling nil) ; See lisp/themes.el
 '(markdown-reference-location 'end)
 '(markdown-asymmetric-header t)
 '(markdown-hide-urls t)
 '(markdown-command "pandoc --standalone -T Preview -f markdown -t html 2> /dev/null"))

(custom-set-faces
 '(markdown-code-face ((t (:background nil))))
 '(markdown-header-delimiter-face ((t (:inherit org-done))))
 '(markdown-header-face-1 ((t (:inherit org-level-1 :height 2.0))))
 '(markdown-header-face-2 ((t (:inherit org-level-2 :height 1.7))))
 '(markdown-header-face-3 ((t (:inherit org-level-3 :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit org-level-4 :height 1.1))))
 '(markdown-header-face-5 ((t (:inherit org-level-5))))
 '(markdown-header-face-6 ((t (:inherit org-level-6)))))

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
  (pjones:open-line-above t))

(defun pjones:markdown-insert-heading-or-item (reverse)
  "Insert a heading or item based on the current context.
If REVERSE is non-nil, do the opposite of what the context says."
  (interactive "P")
  (if (or (markdown-on-heading-p) reverse)
      (progn
        (newline)
        (markdown-insert-header-dwim))
    (pjones:markdown-insert-list-item))
  (evil-insert-state))

(defun pjones:markdown-visual-line ()
  "Don't wrap lines.  Needed for most web forms."
  (interactive)
  (auto-fill-mode -1)
  (whitespace-mode -1)
  (visual-line-mode)
  (visual-fill-mode)
  (darkroom-tentative-mode))

(defun pjones:markdown-follow-thing-at-point (arg)
  "Call (and pass ARG) to `markdown-follow-thing-at-point'."
  (interactive "P")
  (require 'org)
  (cl-letf (((symbol-function 'find-file)
             (lambda (name) (org-open-file name))))
    (markdown-follow-thing-at-point arg)))

(defun pjones:markdown-attach-file (file &optional name)
  "Attach FILE to the current document.

The given FILE will be attached to the current document by adding
it as a git-annex file in the `pjones:markdown-attachments-directory'
directory.  Optionally renaming FILE to NAME."
  (interactive
   (let* ((f (read-file-name "Attach: " nil nil t))
          (n (if current-prefix-arg
                 (read-string "New Name: " (file-name-nondirectory f)))))
     (list f n)))
  (let* ((name (or name (file-name-nondirectory file)))
         (dir (concat default-directory
                      pjones:markdown-attachments-directory))
         (dest (concat (file-name-as-directory dir)
                       (file-name-nondirectory name))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (copy-file file dest)
    (unless (= 0 (call-process "git-annex" nil nil nil "info"))
      (call-process "git-annex" nil nil nil "init"))
    (unless (= 0 (call-process "git-annex" nil nil nil "add" dest))
      (error "Error: git-annex failed"))
    (markdown-insert-reference-definition
     (file-name-nondirectory name)
     (file-relative-name dest))))

(evil-define-operator pjones:markdown-promote (beg end count)
  "Promote, indent, move column left."
  :type line
  :move-point nil
  (interactive "<r><vc>")
  (when (null count) (setq count 1))
  (dotimes (_ count)
    (condition-case nil
        (markdown-promote)
      (user-error
       (evil-shift-left beg end 1)))))

(evil-define-operator pjones:markdown-demote (beg end count)
  "Demote, indent, move column right."
  :type line
  :move-point nil
  (interactive "<r><vc>")
  (when (null count) (setq count 1))
  (dotimes (_ count)
    (condition-case nil
        (markdown-demote)
      (user-error
       (evil-shift-right beg end 1)))))

(defun pjones:markdown-bind-keys ()
  "Bind keys in modes derived from `markdown-mode'."
  (let* ((mode major-mode)
         (map (symbol-value (intern (concat (symbol-name mode) "-map")))))
    (evil-define-key 'insert map
      (kbd "C-j") #'pjones:markdown-insert-heading-or-item
      (kbd "TAB") #'pjones:indent-or-complete)
    (evil-define-key 'motion map
      "[[" #'outline-backward-same-level
      "]]" #'outline-forward-same-level
      "gk" #'outline-up-heading
      "gJ" #'outline-move-subtree-down
      "gK" #'outline-move-subtree-up)
    (evil-define-key 'normal map
      ;; Folding:
      "zo" #'outline-show-entry
      "zO" #'outline-show-subtree
      "zc" #'outline-hide-subtree
      "zr" #'outline-show-all
      "zm" #'outline-hide-sublevels
      ;; Promoting, demoting:
      ">" #'pjones:markdown-demote
      "<" #'pjones:markdown-promote)
    (evil-leader/set-key-for-mode mode
      "g x" #'pjones:markdown-follow-thing-at-point
      "m a" #'pjones:markdown-attach-file
      "m c" #'markdown-preview
      "m f" #'markdown-insert-footnote
      "m j" #'pjones:markdown-insert-heading-or-item
      "m l" #'markdown-insert-link
      "m n" #'markdown-cleanup-list-numbers
      "m p" #'markdown-live-preview-mode
      "m t" #'markdown-insert-table)))

(defun pjones:markdown-mode-hook ()
  "Set up key bindings and other crap for markdown-mode."
  (whitespace-mode)
  (pjones:add-fixme-lock)

  ;; Completion configuration:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-ispell
                                   company-dabbrev)))

(add-hook 'markdown-mode-hook 'pjones:markdown-mode-hook)
(add-hook 'markdown-mode-hook 'pjones:markdown-bind-keys)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; markdown-mode-conf.el ends here
