;; Specific settings for the org-mode files I use to run my business

;; The notes file and options
(setq
 org-directory          "~/Documents/pmade/planning/"
 org-default-notes-file (concat org-directory "clients/pmade.org")
 org-combined-agenda-icalendar-file (concat org-directory "export/agenda.ics")
 org-reverse-note-order t               ; put new notes on top
 org-archive-location  "archive/%s::")

;; Agenda Files and Agenda Settings
(setq org-agenda-files
      (append
       (directory-files (concat (expand-file-name org-directory) "/clients") t "\\.org$")
       (directory-files (concat (expand-file-name org-directory) "/tasks") t "\\.org$")))

(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; Exporting
(setq
 org-export-html-style-default ""
 org-export-html-style-extra ""
 org-export-html-style (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" pmade-print-css "\"/>"))

(defun pmade:org-remove-redundant-heading-markers ()
  "Called from an export buffer, removes leading stars so that the first heading in the export has only one star."
  (let ((reduce-by 0)
        (remove-regex "^"))
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (search-forward-regexp "^\\*")
        (beginning-of-line)
        (when (looking-at "^\\(\\*+\\)[ \t]+")
          (setq reduce-by (- (match-end 1) (point))))
        (when (not (= 0 reduce-by))
          (setq remove-regex (concat remove-regex (regexp-quote (make-string reduce-by ?*))))
          (forward-line 1) ; leave the top heading alone (org must ignore it as well)
          (while (re-search-forward remove-regex nil t)
            (replace-match "" nil nil)
            (forward-line 1)))))))

(add-hook 'org-export-preprocess-hook 'pmade:org-remove-redundant-heading-markers)

;; Org and Remember Mode
(require 'remember)
(org-remember-insinuate)

(setq org-remember-templates
      (list
       (list "Client Tasks"   ?t "** TODO %?\n   %i" (concat org-directory "clients/pmade.org")  "Tasks")
       (list "General Tasks"  ?g "** TODO %?\n   %i" (concat org-directory "tasks/general.org")  "Tasks")
       (list "Devenv Tasks"   ?d "** TODO %?\n   %i" (concat org-directory "tasks/devenv.org")   "Tasks")))
