;;; dired-conf.el --- Settings for `dired-mode'.

;;; Commentary:
;;
;; Settings for `dired-mode'.

;;; Code:
(require 'dired)
(require 'org)
(require 'dired-x)
(require 'dired-aux)
(require 'dired-filter)
(require 'dired-narrow)
(require 'dired-sidebar)
(require 'dired-subtree)

;; Always run commands asynchronously:
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; Settings:
(custom-set-variables
  '(dired-listing-switches "-lhA --literal --ignore=.git --group-directories-first")
  '(dired-auto-revert-buffer t)
  '(dired-isearch-filenames t)
  '(dired-hide-details-hide-symlink-targets nil)
  '(dired-filter-prefix "/")
  '(dired-filter-mark-prefix "M")
  '(dired-subtree-use-backgrounds nil)
  '(dired-subtree-line-prefix-face nil)
  '(dired-sidebar-theme (quote nerd))
  '(dired-sidebar-refresh-on-special-commands nil)
  '(dired-sidebar-follow-file-idle-delay 1)
  '(dired-sidebar-tui-update-delay 0.5)
  '(dired-sidebar-pop-to-sidebar-on-toggle-open nil)
  '(dired-sidebar-should-follow-file t))

(defvar pjones:dired-subtree-line-prefix "  ·"
  "Subdirectory prefix for dired-subtree.")

(defun pjones:dired-insert-or-visit ()
  "If point is on a directory, insert that directory into the
current dired buffer.  Otherwise visit the file under point."
  (interactive)
  (let ((name (dired-get-file-for-visit)))
    (if (file-directory-p name) (dired-subtree-insert)
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

(defun pjones:dired-find-file ()
  "Use the current dired directory as a starting point for `find-file'."
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (call-interactively 'find-file)))

(defhydra hydra-dired (:hint nil) "
 ^Narrowing^        ^Marking^           ^Operations^
-----------------------------------------------------------
  _/ n_: name        _M n_: name         _o_: noccur marked
  _/ r_: regexp      _M r_: regexp       _q_: query replace
  _/ ._: ext         _M ._: ext          _C_: copy
  _/ f_: files       _M f_: files        _d_: delete
  _/ d_: dirs        _M d_: dirs         _R_: rename
  _/ s_: symlinks    _M s_: symlinks     _T_: touch
  _/ m_: mode        _M m_: mode         _S_: symlink
  _/ p_: pop one     _M u_: none         _F_: find
  _/ /_: pop all     _M a_: all
"
  ("/ n" dired-filter-by-name)
  ("/ r" dired-filter-by-regexp)
  ("/ ." dired-filter-by-extension)
  ("/ f" dired-filter-by-file)
  ("/ d" dired-filter-by-directory)
  ("/ s" dired-filter-by-symlink)
  ("/ m" dired-filter-by-mode)
  ("/ p" dired-filter-pop)
  ("/ /" dired-filter-pop-all)
  ("M n" dired-filter-mark-by-name)
  ("M r" dired-filter-mark-by-regexp)
  ("M ." dired-filter-mark-by-extension)
  ("M f" dired-filter-mark-by-file)
  ("M d" dired-filter-mark-by-directory)
  ("M s" dired-filter-mark-by-symlink)
  ("M m" dired-filter-mark-by-mode)
  ("M u" dired-unmark-all-marks)
  ("M a" pjones:dired-mark-all-files)
  ("o" noccur-dired)
  ("q" dired-do-query-replace-regexp)
  ("C" dired-do-copy)
  ("d" dired-flag-file-deletion)
  ("R" dired-do-rename)
  ("T" dired-do-touch)
  ("S" dired-do-relsymlink)
  ("F" find-dired))

(defun pjones:dired-sidebar-mode ()
  "Set up `dired-sidebar-mode'."
  ;; Fix an issue caused by late loading of variables.
  (setq-local dired-subtree-line-prefix pjones:dired-subtree-line-prefix))

(defun pjones:dired-load-hook ()
  "Set up `dired-mode'."
  (dired-hide-details-mode) ;; Hide details by default
  (setq dired-subtree-line-prefix pjones:dired-subtree-line-prefix)

  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "<return>") #'pjones:dired-insert-or-visit
    "gq"             #'dired-do-query-replace-regexp
    "go"             #'noccur-dired
    "zc"             #'dired-subtree-remove)

  (let ((map dired-mode-map))
    (define-key map (kbd "C-x C-f")  #'pjones:dired-find-file)
    (define-key map (kbd "C-x n s")  #'dired-subtree-narrow)
    (define-key map (kbd "C-c C-h")  #'hydra-dired/body)))

(add-hook 'dired-mode-hook 'pjones:dired-load-hook)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(add-hook 'dired-mode-hook 'dired-filter-mode)
(add-hook 'dired-after-readin-hook 'pjones:dired-remove-total-lines)
(add-hook 'dired-sidebar-mode-hook 'pjones:dired-sidebar-mode)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; dired-conf.el ends here
