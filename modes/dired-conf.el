;;; dired-conf.el --- Settings for `dired-mode'. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Settings for `dired-mode'.

;;; Code:

(require 'dired)
(require 'dired-x)

;; Make the linter happy:
(declare-function diff-hl-dired-mode "diff-hl-dired")
(declare-function dired-filter-mode "dired-filter")
(declare-function dired-subtree--get-ov "dired-subtree")
(declare-function dired-subtree-next-sibling "dired-subtree")
(declare-function dired-subtree-previous-sibling "dired-subtree")
(declare-function dired-subtree-toggle "dired-subtree")
(declare-function dired-subtree-up "dired-subtree")
(declare-function magit-clone-regular "magit-clone")
(declare-function noccur-dired "noccur")
(declare-function org-open-file "org")
(declare-function wdired-finish-edit "wdired")
(defvar magit-clone-default-directory)

(defmacro pjones:dired-cwd-do (func)
  "Call FUNC inside the current file's directory."
  `(lambda ()
     (interactive)
     (let ((default-directory (dired-current-directory)))
       (call-interactively ,func))))

(defun pjones:dired-magit-clone-here ()
  "Use `magit-clone-regular' in the current directory."
  (interactive)
  (let ((magit-clone-default-directory (dired-current-directory)))
    (call-interactively #'magit-clone-regular)))

;; Settings:
(custom-set-variables
 '(diff-hl-dired-extra-indicators nil)
 '(dired-auto-revert-buffer t)
 '(dired-create-destination-dirs 'ask)
 '(dired-do-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-filter-mark-prefix nil)
 '(dired-filter-prefix nil)
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-isearch-filenames t)
 '(dired-listing-switches "-lhA --literal --group-directories-first")
 '(dired-subtree-line-prefix-face 'subtree)
 '(dired-subtree-use-backgrounds nil)
 '(dired-vc-rename-file t))

(let ((map dired-mode-map))
  (define-key map (kbd "!") (pjones:dired-cwd-do #'dired-do-shell-command))
  (define-key map (kbd "&") (pjones:dired-cwd-do #'dired-do-async-shell-command))
  (define-key map (kbd "* a") #'pjones:dired-mark-all-files)
  (define-key map (kbd "<return>") #'pjones:dired-insert-or-visit)
  (define-key map (kbd "c") #'pjones:dired-magit-clone-here)
  (define-key map (kbd "C-c C-c") #'pjones:dired-toggle-wdired)
  (define-key map (kbd "C-c M-w") #'pjones:dired-copy-filename-as-kill)
  (define-key map (kbd "C-c M-W") (pjones:dired-cwd-do 'pjones:kill-directory-name))
  (define-key map (kbd "C-i") #'pjones:dired-insert-or-visit)
  (define-key map (kbd "C-x C-f") (pjones:dired-cwd-do 'find-file))
  (define-key map (kbd "M-n") #'dired-subtree-next-sibling)
  (define-key map (kbd "M-p") #'pjones:dired-subtree-up-or-prev)
  (define-key map (kbd "M-s o") #'noccur-dired)
  (define-key map (kbd "X") #'dired-do-compress-to))

(defun pjones:dired-subtree-up-or-prev (&optional arg)
  "Move point up a directory, or to the previous sibling.
When in a nested directory, move up to the parent.  If at the top of
the hierarchy then move to the previous line.
ARG is the number of lines to jump."
  (interactive "p")
  (require 'dired-subtree)
  (if-let (ov (dired-subtree--get-ov))
      (dired-subtree-up arg)
    (dired-subtree-previous-sibling arg)))

(defun pjones:dired-copy-filename-as-kill (&optional path)
  "Copy file name or entire PATH."
  (interactive "P")
  (if path
      (dired-copy-filename-as-kill '(nil . nil))
    (dired-copy-filename-as-kill)))

(defun pjones:dired-insert-or-visit ()
  "Visit the file at point.

If point is on a directory, insert that directory into the current
Dired buffer.  Otherwise visit the file under point."
  (interactive)
  (require 'dired-subtree)
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

(defun pjones:dired-toggle-wdired ()
  "Toggle `wdired-mode'."
  (interactive)
  (require 'wdired)
  (if (eq major-mode 'wdired-mode)
      (wdired-finish-edit)
    (wdired-change-to-wdired-mode)))

(defun pjones:dired-mode-hook ()
  "Set up `dired-mode' buffers."
  (setq-local diff-hl-side 'left)
  (dired-hide-details-mode)
  (dired-filter-mode)
  (diff-hl-dired-mode))

(add-hook 'dired-mode-hook #'pjones:dired-mode-hook)
(add-hook 'dired-after-readin-hook #'pjones:dired-remove-total-lines)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; dired-conf.el ends here
