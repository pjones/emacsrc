;;; dired-conf.el --- Settings for `dired-mode'.

;;; Commentary:
;;
;; Settings for `dired-mode'.

;;; Code:
(require 'dired)
(require 'dired-x)
(require 'evil)
(require 'evil-leader)

(eval-when-compile
  (require 'dired-filter)
  (require 'dired-narrow)
  (require 'dired-subtree)
  (require 'dired-x)
  (require 'noccur)

  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "../lisp/macros")))

(declare-function org-open-file "org")

(defmacro pjones:dired-cwd-do (func)
  "Call FUNC inside the current file's directory."
  `(lambda ()
     (interactive)
     (let ((default-directory (dired-current-directory)))
       (call-interactively ,func))))

;; Settings:
(custom-set-variables
  '(dired-listing-switches "-lhA --literal --ignore=.git --group-directories-first")
  '(dired-auto-revert-buffer t)
  '(dired-isearch-filenames t)
  '(dired-hide-details-hide-symlink-targets nil)
  '(dired-dwim-target t)
  '(dired-filter-prefix nil)
  '(dired-filter-mark-prefix nil)
  '(dired-subtree-line-prefix-face 'subtree)
  '(dired-subtree-use-backgrounds nil))

;; A few extra key bindings:
(evil-leader/set-key-for-mode 'dired-mode
  "f f"   (pjones:dired-cwd-do 'find-file)
  "m G"   #'dired-do-chgrp
  "m / n" #'dired-filter-by-name
  "m / r" #'dired-filter-by-regexp
  "m / ." #'dired-filter-by-extension
  "m / f" #'dired-filter-by-file
  "m / d" #'dired-filter-by-directory
  "m / s" #'dired-filter-by-symlink
  "m / m" #'dired-filter-by-mode
  "m / p" #'dired-filter-pop
  "m / /" #'dired-filter-pop-all
  "m m n" #'dired-filter-mark-by-name
  "m m r" #'dired-filter-mark-by-regexp
  "m m ." #'dired-filter-mark-by-extension
  "m m f" #'dired-filter-mark-by-file
  "m m d" #'dired-filter-mark-by-directory
  "m m s" #'dired-filter-mark-by-symlink
  "m m m" #'dired-filter-mark-by-mode
  "m m u" #'dired-unmark-all-marks
  "m m a" #'pjones:dired-mark-all-files
  "m o"   #'noccur-dired
  "m q"   #'dired-do-query-replace-regexp)

(pjones:evil-override-mode dired-mode
  "!" (pjones:dired-cwd-do #'dired-do-shell-command)
  "&" (pjones:dired-cwd-do #'dired-do-async-shell-command)
  (kbd "<return>") #'pjones:dired-insert-or-visit
  "gk" #'dired-subtree-up
  "gj" #'dired-subtree-down
  "gq" #'dired-do-query-replace-regexp
  "gr" #'revert-buffer
  "go" #'noccur-dired
  "yn" #'dired-copy-filename-as-kill
  "yp" '(lambda () (interactive) (dired-copy-filename-as-kill '(nil . nil))))

(defun pjones:dired-insert-or-visit ()
  "Visit the file at point.

If point is on a directory, insert that directory into the current
dired buffer.  Otherwise visit the file under point."
  (interactive)
  (let ((name (dired-get-file-for-visit)))
    (if (file-directory-p name) (dired-subtree-toggle)
      (require 'org)
      (org-open-file name))))

(defun pjones:dired-remove-total-lines ()
  "Remove those useless \"total\" lines from ls."
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (flush-lines "^ *total"))))

(defun pjones:dired-mark-all-files ()
  "Mark all files."
  (interactive)
  (dired-unmark-all-marks)
  (dired-toggle-marks))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)
(add-hook 'dired-mode-hook #'dired-filter-mode)
(add-hook 'dired-after-readin-hook #'pjones:dired-remove-total-lines)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; dired-conf.el ends here
