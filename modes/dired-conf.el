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
(require 'dired-subtree)
(require 'noccur)

(declare-function org-open-file "org")
(declare-function evil-collection-define-key "evil-collection")

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
  '(dired-filter-prefix "/")
  '(dired-filter-mark-prefix "M")
  '(dired-subtree-line-prefix-face 'subtree)
  '(dired-subtree-use-backgrounds nil))

;; A few extra key bindings:
(evil-leader/set-key-for-mode 'dired-mode
  "f"       (pjones:dired-cwd-do 'find-file)
  "DEL / n" #'dired-filter-by-name
  "DEL / r" #'dired-filter-by-regexp
  "DEL / ." #'dired-filter-by-extension
  "DEL / f" #'dired-filter-by-file
  "DEL / d" #'dired-filter-by-directory
  "DEL / s" #'dired-filter-by-symlink
  "DEL / m" #'dired-filter-by-mode
  "DEL / p" #'dired-filter-pop
  "DEL / /" #'dired-filter-pop-all
  "DEL m n" #'dired-filter-mark-by-name
  "DEL m r" #'dired-filter-mark-by-regexp
  "DEL m ." #'dired-filter-mark-by-extension
  "DEL m f" #'dired-filter-mark-by-file
  "DEL m d" #'dired-filter-mark-by-directory
  "DEL m s" #'dired-filter-mark-by-symlink
  "DEL m m" #'dired-filter-mark-by-mode
  "DEL m u" #'dired-unmark-all-marks
  "DEL m a" #'pjones:dired-mark-all-files
  "DEL o"   #'noccur-dired
  "DEL q"   #'dired-do-query-replace-regexp
  "DEL c"   #'dired-do-copy
  "DEL d"   #'dired-flag-file-deletion
  "DEL r"   #'dired-do-rename
  "DEL T"   #'dired-do-touch
  "DEL s"   #'dired-do-relsymlink
  "DEL f"   #'find-dired)

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

(defun pjones:dired-load-hook ()
  "Set up `dired-mode'."
  (dired-hide-details-mode) ;; Hide details by default

  (require 'evil-collection)
  (evil-collection-define-key 'normal 'dired-mode-map
    "!" (pjones:dired-cwd-do 'dired-do-shell-command)
    "&" (pjones:dired-cwd-do 'dired-do-async-shell-command)
    (kbd "<return>") #'pjones:dired-insert-or-visit
    "gk"             #'dired-subtree-up
    "gj"             #'dired-subtree-down
    "gq"             #'dired-do-query-replace-regexp
    "go"             #'noccur-dired
    "yn"             #'dired-copy-filename-as-kill
    "yp"             '(lambda () (interactive) (dired-copy-filename-as-kill '(nil . nil))))

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
