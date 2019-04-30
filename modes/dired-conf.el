;;; dired-conf.el --- Settings for `dired-mode'.

;;; Commentary:
;;
;; Settings for `dired-mode'.

;;; Code:
(require 'dired)
(require 'dired-x)
(require 'dired-aux)
(require 'dired-filter)
(require 'dired-narrow)
(require 'noccur)

(declare-function org-open-file "org")
(declare-function evil-collection-define-key "evil-collection")

;; Settings:
(custom-set-variables
  '(dired-listing-switches "-lhA --literal --ignore=.git --group-directories-first")
  '(dired-auto-revert-buffer t)
  '(dired-isearch-filenames t)
  '(dired-hide-details-hide-symlink-targets nil)
  '(dired-dwim-target t)
  '(dired-filter-prefix "/")
  '(dired-filter-mark-prefix "M"))

(defun pjones:dired-insert-or-visit ()
  "Visit the file at point.

If point is on a directory, insert that directory into the current
dired buffer.  Otherwise visit the file under point."
  (interactive)
  (let ((name (dired-get-file-for-visit)))
    (if (file-directory-p name) (dired-maybe-insert-subdir name)
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

(defmacro pjones:dired-cwd-do (func)
  "Call FUNC inside the current file's directory."
  `(lambda ()
     (interactive)
     (let ((default-directory (dired-current-directory)))
       (call-interactively ,func))))

(defun pjones:dired-load-hook ()
  "Set up `dired-mode'."
  (dired-hide-details-mode) ;; Hide details by default

  (require 'evil-collection)
  (evil-collection-define-key 'normal 'dired-mode-map
    "!" (pjones:dired-cwd-do 'dired-do-shell-command)
    "&" (pjones:dired-cwd-do 'dired-do-async-shell-command)
    (kbd "<return>") #'pjones:dired-insert-or-visit
    "gq"             #'dired-do-query-replace-regexp
    "go"             #'noccur-dired)

  (let ((map dired-mode-map))
    (define-key map (kbd "!")        (pjones:dired-cwd-do 'dired-do-shell-command))
    (define-key map (kbd "&")        (pjones:dired-cwd-do 'dired-do-async-shell-command))
    (define-key map (kbd "C-x C-f")  (pjones:dired-cwd-do 'find-file))))

(add-hook 'dired-mode-hook 'pjones:dired-load-hook)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(add-hook 'dired-mode-hook 'dired-filter-mode)
(add-hook 'dired-after-readin-hook 'pjones:dired-remove-total-lines)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; dired-conf.el ends here
