;;; org-journal-conf.el -- Settings for org-journal
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'org)
(require 'org-journal)
(require 'org-roam)

(declare-function pjones:org-insert-below "./org-conf.el")

(custom-set-variables
 '(org-journal-file-type 'daily)
 '(org-journal-start-on-weekday 1) ; Monday
 `(org-journal-dir ,org-roam-directory)
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-journal-date-format "%A, %B %d %Y")
 '(org-journal-search-result-date-format "%A, %B %d %Y")
 '(org-journal-date-prefix "#+title: Journal Entry for ")
 '(org-journal-time-format "%R ")
 '(org-journal-time-prefix "* ")
 '(org-journal-enable-agenda-integration t)
 '(org-journal-find-file 'find-file)
 '(org-journal-carryover-items "TODO=\"TODO\""))

(defun pjones:org-journal-insert-below (&optional todo)
  "Insert a new plain item or journal entry.
When TODO is non-nil mark the item with a to-do keyword or checkbox."
  (interactive "P")
  (if (org-at-item-p)
      (pjones:org-insert-below todo)
    (org-journal-new-entry nil)
    (when todo (org-todo 'nextset))
    (evil-insert-state)))

(evil-define-key 'insert org-journal-mode-map
  "\C-j" #'pjones:org-journal-insert-below)

;;; org-journal-conf.el ends here
