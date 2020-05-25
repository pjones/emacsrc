;;; org-journal-conf.el -- Settings for org-journal
;;
;;; Commentary:
;;
;;; Code:
(require 'org-journal)
(require 'org-roam)

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

;;; org-journal-conf.el ends here
